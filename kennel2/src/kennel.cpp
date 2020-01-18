#include <iostream>
#include <fstream>
#include <sstream>
#include <random>
#include <algorithm>
#include "libs.h"
#include "root.h"
#include "nojs_root.h"
#include "user.h"
#include "protocol.h"
#include "eventsource.h"
#include "ndjson.h"
#include "permlink.h"
#include "http_client.h"
#include "../../cattleshed/src/syslogstream.cc"
#include "cattleshed_client.h"

namespace cppcms {
    template<>
    struct serialization_traits<json::value> {
        static void load(const std::string& serialized_object, json::value& real_object) {
            std::stringstream ss(serialized_object);
            real_object.load(ss, true, nullptr);
        }
        static void save(const json::value& real_object, std::string& serialized_object) {
            std::stringstream ss;
            real_object.save(ss, cppcms::json::compact);
            serialized_object = ss.str();
        }
    };
}

class kennel : public cppcms::application {
public:
    kennel(cppcms::service &srv) : cppcms::application(srv) {
        {
            auto channel = grpc::CreateChannel("localhost:50051", grpc::InsecureChannelCredentials());
            CattleshedClientManager cm(channel, 1);
            cattleshed::GetVersionRequest req;
            auto client = cm.GetVersion(
                req,
                [](cattleshed::GetVersionResponse resp, grpc::Status status) {},
                [](ggrpc::ClientResponseReaderError error) {});
            auto client2 =
                cm.RunJob([](cattleshed::RunJobResponse resp) {},
                          [](grpc::Status status) {},
                          [](ggrpc::ClientReaderWriterError error) {});
            cattleshed::RunJobRequest req2;
            client2->Write(req2);
            client2->WritesDone();
        }

        // static file serving is for debugging
        // use Apache, nginx and so on in production
        dispatcher().assign("/static/(([a-zA-Z0-9_\\-]+/)*+([a-zA-Z0-9_\\-]+)\\.(js|css|png|gif))", &kennel::serve_file, this, 1, 4);
        dispatcher().assign("/static/(js/jquery.cookie.(js))", &kennel::serve_file, this, 1, 2);
        dispatcher().assign("/static/(js/jquery.url.(js))", &kennel::serve_file, this, 1, 2);
        mapper().assign("static", "/static");

        dispatcher().assign("/compile/?", &kennel::compile, this);
        mapper().assign("compile", "/compile");

        dispatcher().assign("/permlink/?", &kennel::post_permlink, this);
        mapper().assign("permlink", "/permlink");

        dispatcher().assign("/permlink/([a-zA-Z0-9]+)/?", &kennel::get_permlink, this, 1);
        mapper().assign("get-permlink", "/permlink/{1}");

        dispatcher().assign("/login/github/callback", &kennel::get_github_callback, this);

        dispatcher().assign("/api/user.json", &kennel::api_user, this);
        dispatcher().assign("/api/list.json", &kennel::api_list, this);
        dispatcher().assign("/api/compile.json", &kennel::api_compile, this);
        dispatcher().assign("/api/compile.ndjson", &kennel::api_compile_ndjson, this);
        dispatcher().assign("/api/permlink/?", &kennel::api_post_permlink, this);
        dispatcher().assign("/api/permlink/([a-zA-Z0-9]+)/?", &kennel::api_permlink, this, 1);
        dispatcher().assign("/api/template/(.+?)/?", &kennel::api_template, this, 1);
        mapper().assign("api-template", "/api/template/");

        dispatcher().assign("/nojs/(.+?)/compile/?", &kennel::nojs_compile, this, 1);
        mapper().assign("nojs-compile", "/nojs/{1}/compile");
        dispatcher().assign("/nojs/(.+?)/permlink/([a-zA-Z0-9]+)/?", &kennel::nojs_get_permlink, this, 1, 2);
        mapper().assign("nojs-get-permlink", "/nojs/{1}/permlink/{2}");
        dispatcher().assign("/nojs/([a-zA-Z0-9_\\-\\.]+)/?", &kennel::nojs_root, this, 1);
        mapper().assign("nojs-root", "/nojs/{1}");
        dispatcher().assign("/nojs/?", &kennel::nojs_list, this);
        mapper().assign("nojs-list", "/nojs");

        dispatcher().assign("/signout/?", &kennel::signout, this);
        mapper().assign("signout", "/signout");

        dispatcher().assign("/user/(.+?)/?", &kennel::user, this, 1);
        mapper().assign("user", "/user/{1}");

        dispatcher().assign("/?", &kennel::root, this);
        if (srv.settings()["application"]["map_root"].str().empty()) {
            mapper().assign("root", "/");
        } else {
            mapper().assign("root", "");
        }
    }

private:
    cppcms::json::value json_post_data() {
        auto p = request().raw_post_data();
        std::stringbuf sb(std::ios_base::in);
        sb.pubsetbuf(static_cast<char*>(p.first), p.second);
        std::istream is(&sb);
        cppcms::json::value value;
        value.load(is, true, nullptr);
        return value;
    }

    cppcms::json::value get_compiler_infos_or_cache() {
        cppcms::json::value json;
        if (!cache().fetch_data("compiler_infos", json)) {
            if (!cache().fetch_data("compiler_infos_persist", json)) {
                json = default_compiler_infos();
                cache().store_data("compiler_infos", json, 600);
                cache().store_data("compiler_infos_persist", json, -1);
            } else {
                cache().store_data("compiler_infos", json, 600);
                cache().store_data("compiler_infos_persist", json, -1);

                booster::function<cppcms::json::value (std::vector<cppcms::json::value>&)> merge_compiler_infos = &kennel::merge_compiler_infos;
                auto context = get_context();
                get_compiler_infos_async([context, merge_compiler_infos](std::vector<cppcms::json::value>& jsons) {
                    // On old gcc (4.6) bug.
                    // error: ‘this’ was not captured for this lambda function
                    // auto json = kennel::merge_compiler_infos(jsons);
                    auto json = merge_compiler_infos(jsons);
                    context->cache().store_data("compiler_infos", json, 600);
                    context->cache().store_data("compiler_infos_persist", json, -1);
                });
            }
        }
        return json;
    }

public:
    static cppcms::json::value& default_compiler_infos() {
        static cppcms::json::value value;
        return value;
    }
    static std::vector<cppcms::json::value> get_compiler_infos(cppcms::service& service) {
        std::vector<protocol> protos = {
            protocol{"Version", ""},
        };
        std::vector<cppcms::json::value> values;
        auto size = service.settings()["application"]["cattleshed"].array().size();

        for (auto i = 0; i < static_cast<int>(size); i++) {
            std::string json;
            send_command(service, i, protos, [&json](const booster::system::error_code& e, const protocol& proto) {
                if (e)
                    return (void)(std::clog << e.message() << std::endl);
                json = proto.contents;
            }, 1);
            std::stringstream ss(json);
            cppcms::json::value value;
            value.load(ss, true, nullptr);
            values.push_back(value);
        }
        return values;
    }
    static cppcms::json::value merge_compiler_infos(const std::vector<cppcms::json::value>& infos) {
        std::vector<cppcms::json::value> compilers;
        cppcms::json::object templates;
        for (auto i = 0; i < static_cast<int>(infos.size()); i++) {
            for (const auto& info: infos[i]["compilers"].array()) {
                auto name = info["name"];
                auto it = std::find_if(compilers.begin(), compilers.end(), [name](cppcms::json::value& v) { return v["name"] == name; });

                if (it != compilers.end()) {
                    // update
                    *it = info;
                    (*it)["provider"].number(i);
                } else {
                    // new
                    cppcms::json::value value = info;
                    value.object()["provider"].number(i);
                    compilers.push_back(std::move(value));
                }
            }
            for (const auto& info: infos[i]["templates"].object()) {
                templates[info.first] = info.second;
            }
        }
        cppcms::json::value r;
        r.set("compilers", compilers);
        r.set("templates", templates);
        return r;
    }

private:
    std::vector<cppcms::json::value> get_compiler_infos() {
        return get_compiler_infos(service());
    }
    template<class F>
    void get_compiler_infos_async(const F& callback) {
        std::vector<protocol> protos = {
            protocol{"Version", ""},
        };
        typedef booster::shared_ptr<cppcms::json::value> json_ptr;
        typedef booster::shared_ptr<std::vector<booster::shared_ptr<cppcms::json::value>>> result_type;
        auto results = result_type(new result_type::value_type());
        auto size = service().settings()["application"]["cattleshed"].array().size();
        results->resize(size);

        for (auto i = 0; i < static_cast<int>(size); i++) {
            send_command_async(service(), i, protos, [callback, results, i](const booster::system::error_code& e, const protocol& proto) {
                if (e)
                    return (void)(std::clog << e.message() << std::endl);
                std::stringstream ss(proto.contents);
                json_ptr value(new cppcms::json::value());
                value->load(ss, true, nullptr);
                //value->save(std::clog, cppcms::json::readable);
                (*results)[i] = value;
                auto completed = std::all_of(results->begin(), results->end(), [](const json_ptr& v) { return v; });
                if (completed) {
                    std::vector<cppcms::json::value> jsons(results->size());
                    for (auto i = 0; i < static_cast<int>(jsons.size()); i++) {
                        jsons[i] = std::move(*(*results)[i]);
                    }
                    callback(jsons);
                }
            }, 1);
        }
    }

    bool ensure_method_get() {
        if (request().request_method() == "HEAD") {
            return true;
        }
        if (request().request_method() == "GET") {
            return true;
        }

        if (request().request_method() == "OPTIONS") {
            response().status(200);
            response().allow("OPTIONS, GET, HEAD");
            response().set_header("Access-Control-Allow-Origin", "*");
            auto headers = request().getenv("HTTP_ACCESS_CONTROL_REQUEST_HEADERS");
            if (!headers.empty())
                response().set_header("Access-Control-Allow-Headers", headers);
            return false;
        } else {
            response().status(405);
            response().allow("OPTIONS, GET, HEAD");
            return false;
        }
    }

    bool ensure_method_post() {
        if (request().request_method() == "POST") {
            return true;
        }

        if (request().request_method() == "OPTIONS") {
            response().status(200);
            response().allow("OPTIONS, POST");
            response().set_header("Access-Control-Allow-Origin", "*");
            //for (auto it = request().getenv().begin(); it != request().getenv().end(); ++it)
            //    std::cout << it->first << ": " << it->second << std::endl;
            auto headers = request().getenv("HTTP_ACCESS_CONTROL_REQUEST_HEADERS");
            if (!headers.empty())
                response().set_header("Access-Control-Allow-Headers", headers);
            return false;
        } else {
            response().status(405);
            response().allow("OPTIONS, POST");
            return false;
        }
    }

    cppcms::json::value authenticate(std::string access_token) {
        if (access_token.empty()) {
            return cppcms::json::value();
        } else {
            std::vector<std::string> headers = {
                "Content-Type: application/json; charset=utf-8",
                "Accept: application/json",
                "User-Agent: Wandbox",
            };
            auto resp = http_client::get("https://api.github.com/user?access_token=" + access_token, headers);
            if (resp.status_code != 200) {
                // maybe session is expired
                return cppcms::json::value();
            } else {
                // got following data
                /*
                  {
                    "login": "melpon",
                    "id": 816539,
                    "avatar_url": "https://avatars0.githubusercontent.com/u/816539?v=3",
                    "gravatar_id": "",
                    "url": "https://api.github.com/users/melpon",
                    "html_url": "https://github.com/melpon",
                    "followers_url": "https://api.github.com/users/melpon/followers",
                    "following_url": "https://api.github.com/users/melpon/following{/other_user}",
                    "gists_url": "https://api.github.com/users/melpon/gists{/gist_id}",
                    "starred_url": "https://api.github.com/users/melpon/starred{/owner}{/repo}",
                    "subscriptions_url": "https://api.github.com/users/melpon/subscriptions",
                    "organizations_url": "https://api.github.com/users/melpon/orgs",
                    "repos_url": "https://api.github.com/users/melpon/repos",
                    "events_url": "https://api.github.com/users/melpon/events{/privacy}",
                    "received_events_url": "https://api.github.com/users/melpon/received_events",
                    "type": "User",
                    "site_admin": false,
                    "name": "melpon",
                    "company": null,
                    "blog": "http://melpon.org",
                    "location": "Tokyo, Japan",
                    "email": "shigemasa7watanabe+github@gmail.com",
                    "hireable": null,
                    "bio": null,
                    "public_repos": 28,
                    "public_gists": 28,
                    "followers": 97,
                    "following": 16,
                    "created_at": "2011-05-29T02:19:41Z",
                    "updated_at": "2017-06-04T05:56:10Z"
                  }
                */
                cppcms::json::value json;
                std::stringstream ss(resp.body);
                json.load(ss, true, nullptr);

                return json;
            }
        }
    }

    cppcms::json::value get_author(std::string username) {
        cppcms::json::value json;

        std::vector<std::string> headers = {
            "Content-Type: application/json; charset=utf-8",
            "Accept: application/json",
            "User-Agent: Wandbox",
        };
        auto resp = http_client::get("https://api.github.com/users/" + username, headers);
        if (resp.status_code == 200) {
            std::stringstream ss(resp.body);
            json.load(ss, true, nullptr);
        }
        return json;
    }

    void root() {
        if (!ensure_method_get()) {
            return;
        }

        content::root c(service());
        c.set_compiler_infos(get_compiler_infos_or_cache()["compilers"]);

        permlink pl(service());
        auto json = authenticate(pl.get_github_access_token(session()["access_token"]));
        if (json.is_undefined()) {
            session().erase("access_token");
        } else {
            content::root::login_info_t info;
            info.name = json["login"].str();
            if (!json["avatar_url"].is_null()) {
                info.avatar_url = json["avatar_url"].str() + "&s=20";
            }
            c.set_login(info);
        }

        render("root", c);
    }

    void signout() {
        if (!ensure_method_get()) {
            return;
        }
        session().erase("access_token");

        std::stringstream root_ss;
        mapper().map(root_ss, "root");
        response().status(302);
        response().set_redirect_header(root_ss.str());
    }

    static std::vector<protocol> make_protocols(const cppcms::json::value& value) {
        std::vector<protocol> protos = {
            protocol{"Control", "compiler=" + value["compiler"].str()},
            protocol{"StdIn", value.get("stdin", "")},
            protocol{"CompilerOptionRaw", value.get("compiler-option-raw", "")},
            protocol{"RuntimeOptionRaw", value.get("runtime-option-raw", "")},
            protocol{"Source", value["code"].str()},
            protocol{"CompilerOption", value.get("options", "")},
        };
        const auto& codes = value.find("codes");
        if (!codes.is_undefined()) {
            for (const auto& code: codes.array()) {
                protos.push_back(protocol{"SourceFileName", code["file"].str()});
                protos.push_back(protocol{"Source", code["code"].str()});
            }
        }
        protos.push_back(protocol{"Control", "run"});
        return protos;
    }
    static cppcms::json::value form_to_json(const cppcms::http::request::form_type& form) {
        cppcms::json::value result;
        auto set_if_exists = [&result, &form](const std::string& result_key, const std::string& form_key) {
            auto it = form.find(form_key);
            if (it != form.end())
                result[result_key].str(it->second);
        };
        auto starts_with = [](const std::string& str, const std::string& target) {
            return std::equal(target.begin(), target.end(), str.begin());
        };
        auto join = [](const std::vector<std::string>& v, const std::string& sep) -> std::string {
            std::string result;
            if (v.empty()) return result;
            result += v.front();
            for (int i = 1; i < (int)v.size(); i++) result += sep + v[i];
            return result;
        };

        std::vector<std::string> options;
        for (auto&& kv: form) {
            if (starts_with(kv.first, "checkbox-") || starts_with(kv.first, "select-")) {
                options.push_back(kv.second);
            }
        }
        set_if_exists("compiler", "compiler");
        set_if_exists("stdin", "stdin");
        set_if_exists("compiler-option-raw", "compiler-option-raw");
        set_if_exists("runtime-option-raw", "runtime-option-raw");
        set_if_exists("code", "code");
        if (not options.empty()) {
            result["options"] = join(options, ",");
        }
        {
            auto it = form.find("save");
            if (it != form.end())
                result["save"] = true;
        }
        return result;
    }
    template<class Content>
    static void set_twitter_if_info_exists(Content& c, const cppcms::json::value& info, std::string code) {
        if (!info.is_undefined()) {
            std::string title = "[" + info["language"].str() + "] " + info["display-name"].str() + " " + info["version"].str();
            std::string description = std::move(code);
            c.set_twitter(std::move(title), std::move(description));
        }
    }

    void compile() {
        if (!ensure_method_post()) {
            return;
        }

        auto value = json_post_data();
        auto protos = make_protocols(value);
        auto compiler = value["compiler"].str();
        auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
        // find compiler info.
        auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
            [&compiler](cppcms::json::value& v) {
                return v["name"].str() == compiler;
            });
        // error if the compiler is not found
        if (it == compiler_infos.array().end()) {
            throw std::exception();
        }
        auto index = static_cast<std::size_t>((*it)["provider"].number());

        auto es = booster::shared_ptr<eventsource>(new eventsource(release_context()));
        es->send_header();
        send_command_async(service(), index, protos, [es](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);
            es->send_data(proto.command + ":" + proto.contents, true);
            //std::clog << proto.command << ":" << proto.contents << std::endl;
        });
    }
    static std::string make_random_name(std::size_t length) {
        std::string name;
        std::random_device seed_gen;
        std::mt19937 engine(seed_gen());
        std::uniform_int_distribution<> dist(0, 127);
        while (name.size() < length) {
            auto c = (char)dist(engine);
            if (('0' <= c && c <= '9') ||
                ('a' <= c && c <= 'z') ||
                ('A' <= c && c <= 'Z')) {
                name.push_back(c);
            }
        }
        return name;
    }
    void post_permlink() {
        if (!ensure_method_post()) {
            return;
        }

        auto value = json_post_data();

        permlink pl(service());

        cppcms::json::value auth;
        if (value["login"].boolean()) {
            auth = authenticate(pl.get_github_access_token(session()["access_token"]));
            if (auth.is_undefined()) {
                // user is expecting to logged in but actually not.
                response().status(400);
                return;
            }
        }

        auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
        // find compiler info.
        auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
            [&value](cppcms::json::value& v) {
                return v["name"].str() == value["compiler"].str();
            });
        // error if the compiler is not found
        if (it == compiler_infos.array().end()) {
            response().status(400);
            return;
        }

        std::string permlink_name = make_random_name(16);
        pl.make_permlink(permlink_name, value, *it, auth);

        response().content_type("application/json");
        cppcms::json::value result;
        result["permlink"] = permlink_name;

        auto settings = service().settings()["application"];
        auto scheme = settings["scheme"].str();
        auto domain = settings["domain"].str();
        auto root = settings["map_root"].str();
        result["url"] = scheme + "://" + domain + root + "/permlink/" + permlink_name;

        result.save(response().out(), cppcms::json::compact);
    }
    void get_permlink(std::string permlink_name) {
        if (!ensure_method_get()) {
            return;
        }

        content::root c(service());
        c.set_compiler_infos(get_compiler_infos_or_cache()["compilers"]);

        permlink pl(service());
        auto json = authenticate(pl.get_github_access_token(session()["access_token"]));
        if (json.is_undefined()) {
            session().erase("access_token");
        } else {
            content::root::login_info_t info;
            info.name = json["login"].str();
            if (!json["avatar_url"].is_null()) {
                info.avatar_url = json["avatar_url"].str() + "&s=20";
            }
            c.set_login(info);
        }

        auto result = pl.get_permlink(permlink_name);

        std::string username = result["github_user"].str();

        cppcms::json::object author;
        author["created_at"] = result["created_at"];
        if (!username.empty()) {
            auto json = get_author(username);
            if (json["avatar_url"].type() == cppcms::json::is_string) {
                std::string avatar_url = json["avatar_url"].str() + "&s=40";
                author["avatar_url"] = avatar_url;
                author["username"] = username;
                std::stringstream url_ss;
                mapper().map(url_ss, "user", username);
                author["username_url"] = url_ss.str();
            }
        }

        result["author"].object(author);

        std::stringstream ss;
        result.save(ss, cppcms::json::compact);

        c.set_permlink(ss.str());

        auto info = result["compiler-info"];
        set_twitter_if_info_exists(c, info, result["code"].str());

        render("root", c);
    }

    void serve_file(std::string file_name, std::string ext) {
        auto static_dir = service().settings()["application"]["static_dir"].str();
        std::ifstream f((static_dir + "/" + file_name).c_str());

        if (!f) {
            response().status(404);
        } else {
            std::string content_type =
                ext == "js" ? "text/javascript" :
                ext == "css" ? "text/css" :
                ext == "gif" ? "image/gif" :
                ext == "png" ? "image/png" :
                               "";
            response().content_type(content_type);
            response().out() << f.rdbuf();
        }
    }

    void api_user() {
        if (!ensure_method_get()) {
            return;
        }

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");

        auto access_token = get_query_string("session");
        permlink pl(service());
        auto auth = authenticate(pl.get_github_access_token(access_token));
        cppcms::json::value resp;
        if (auth.is_undefined()) {
            resp["login"] = false;
        } else {
            resp["login"] = true;
            resp["username"] = auth["login"].str();
        }
        resp.save(response().out(), cppcms::json::compact);
    }

    void api_post_permlink() {
        response().set_header("Access-Control-Allow-Origin", "*");
        post_permlink();
    }

    void api_list() {
        if (!ensure_method_get()) {
            return;
        }

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        get_compiler_infos_or_cache()["compilers"].save(response().out(), cppcms::json::compact);
    }

    static void update_compile_result(cppcms::json::value& result, const protocol& proto) {
        static auto append = [](cppcms::json::value& v, const std::string& str) {
            if (v.is_undefined())
                v = str;
            else
                v.str() += str;
        };
        if (false) {
        } else if (proto.command == "CompilerMessageS") {
            append(result["compiler_output"], proto.contents);
            append(result["compiler_message"], proto.contents);
        } else if (proto.command == "CompilerMessageE") {
            append(result["compiler_error"], proto.contents);
            append(result["compiler_message"], proto.contents);
        } else if (proto.command == "StdOut") {
            append(result["program_output"], proto.contents);
            append(result["program_message"], proto.contents);
        } else if (proto.command == "StdErr") {
            append(result["program_error"], proto.contents);
            append(result["program_message"], proto.contents);
        } else if (proto.command == "ExitCode") {
            append(result["status"], proto.contents);
        } else if (proto.command == "Signal") {
            append(result["signal"], proto.contents);
        } else {
            //append(result["error"], proto.contents);
        }
    }
    void api_compile() {
        if (!ensure_method_post()) {
            return;
        }

        auto result = api_compile_internal(json_post_data());

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        result.save(response().out(), cppcms::json::readable);
    }
    cppcms::json::value api_compile_internal(cppcms::json::value value) {
        auto compiler = value["compiler"].str();
        auto save = value.get("save", false);
        auto protos = make_protocols(value);
        auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
        // find compiler info.
        auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
            [&compiler](cppcms::json::value& v) {
                return v["name"].str() == compiler;
            });
        // error if the compiler is not found
        if (it == compiler_infos.array().end()) {
            throw std::exception();
        }
        auto index = (*it)["provider"].number();

        cppcms::json::value result;
        cppcms::json::value outputs;
        outputs.array({});
        send_command(service(), index, protos, [this, &result, &outputs, save](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);

            update_compile_result(result, proto);

            if (save) {
                cppcms::json::value v;
                v["type"] = proto.command;
                v["data"] = proto.contents;
                outputs.array().push_back(v);
            }
        });

        if (save) {
            permlink pl(service());
            std::string permlink_name = make_random_name(16);
            value["results"] = outputs;
            pl.make_permlink(permlink_name, value, *it, cppcms::json::value());
            result["permlink"] = permlink_name;

            auto settings = service().settings()["application"];
            auto scheme = settings["scheme"].str();
            auto domain = settings["domain"].str();
            auto root = settings["map_root"].str();
            result["url"] = scheme + "://" + domain + root + "/permlink/" + permlink_name;
        }

        return result;
    }
    void api_compile_ndjson() {
        if (!ensure_method_post()) {
            return;
        }

        auto value = json_post_data();
        auto protos = make_protocols(value);
        auto compiler = value["compiler"].str();
        auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
        // find compiler info.
        auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
            [&compiler](cppcms::json::value& v) {
                return v["name"].str() == compiler;
            });
        // error if the compiler is not found
        if (it == compiler_infos.array().end()) {
            throw std::exception();
        }
        auto index = static_cast<std::size_t>((*it)["provider"].number());

        auto nd = booster::shared_ptr<ndjson>(new ndjson(release_context()));
        nd->send_header();
        nd->context->response().set_header("Access-Control-Allow-Origin", "*");
        send_command_async(service(), index, protos, [nd, value](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);
            cppcms::json::value json;
            json["type"] = proto.command;
            json["data"] = proto.contents;
            nd->send(json, true);
            //std::clog << proto.command << ":" << proto.contents << std::endl;
        });
    }
    void api_permlink(std::string permlink_name) {
        if (!ensure_method_get()) {
            return;
        }

        permlink pl(service());
        auto value = pl.get_permlink(permlink_name);
        cppcms::json::value result;
        cppcms::json::value results;

        result.object({});
        results.array({});

        // １件も出力が存在しない場合は results フィールドそのものが存在しなくなるのでチェックする
        if (value.object().find("results") != value.object().end()) {
          for (auto&& output: value["results"].array()) {
              update_compile_result(result, protocol{output["type"].str(), output["data"].str()});
          }
          results = value["results"].array();
          value.object().erase("results");
        }

        cppcms::json::value resp;
        resp["parameter"] = value;
        resp["result"] = result;
        resp["results"] = results;

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        resp.save(response().out());
    }

    void api_template(std::string template_name) {
        if (!ensure_method_get()) {
            return;
        }

        auto t = get_compiler_infos_or_cache()["templates"].find(template_name);

        if (t.is_undefined()) {
            response().status(400);
            response().out() << "bad template_name: " << template_name << std::endl;
            return;
        }

        cppcms::json::value result;

        result["code"] = t["code"];

        cppcms::json::value opt;
        opt = t.find("stdin");
        if (!opt.is_undefined()) result["stdin"] = opt;
        opt = t.find("options");
        if (!opt.is_undefined()) result["options"] = opt;
        opt = t.find("compiler-option-raw");
        if (!opt.is_undefined()) result["compiler-option-raw"] = opt;
        opt = t.find("runtime-option-raw");
        if (!opt.is_undefined()) result["runtime-option-raw"] = opt;
        opt = t.find("codes");
        if (!opt.is_undefined()) result["codes"] = opt;

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        result.save(response().out(), cppcms::json::readable);
    }

    void get_github_callback() {
        if (!ensure_method_get()) {
            return;
        }

        auto code = get_query_string("code");
        auto settings = service().settings()["application"]["github"];

        cppcms::json::object obj;
        obj["client_id"] = settings["client_id"].str();

        // get client secret from a file
        {
            std::ifstream fs(settings["client_secret_file"].str());
            std::stringstream secret_ss;
            secret_ss << fs.rdbuf();
            auto secret = secret_ss.str();
            auto it = std::find_if(secret.rbegin(), secret.rend(), [](char c) { return !std::isspace(c); }).base();
            secret.erase(it, secret.end());
            obj["client_secret"].str(secret);
        }

        obj["code"] = code;
        obj["accept"] = "json";

        cppcms::json::value body;
        body.object(obj);
        std::stringstream body_ss;
        body.save(body_ss, cppcms::json::compact);

        auto redirect_uri = get_query_string("redirect_uri");
        std::string query_string = "";

        std::vector<std::string> headers = {
            "Content-Type: application/json; charset=utf-8",
            "Accept: application/json",
        };
        auto resp = http_client::post("https://github.com/login/oauth/access_token", std::move(headers), std::move(body_ss.str()));
        if (resp.status_code == 200) {
            cppcms::json::value resp_json;
            std::stringstream resp_ss(resp.body);
            resp_json.load(resp_ss, true, nullptr);

            auto access_token = resp_json.object()["access_token"].str();

            auto auth = authenticate(access_token);
            if (!auth.is_undefined()) {
                permlink pl(service());
                auto wandbox_access_token = make_random_name(32);
                wandbox_access_token = pl.login_github(auth["login"].str(), access_token, wandbox_access_token);

                session()["access_token"] = wandbox_access_token;
                query_string = "?session=" + wandbox_access_token;
            }
        }

        if (redirect_uri.empty()) {
            std::stringstream root_ss;
            mapper().map(root_ss, "root");
            response().set_redirect_header(root_ss.str());
        } else {
            response().set_redirect_header(redirect_uri + query_string);
        }
    }

    void nojs_list() {
        if (!ensure_method_get()) {
            return;
        }

        content::root c(service());
        c.set_compiler_infos(get_compiler_infos_or_cache()["compilers"]);
        render("nojs_list", c);
    }

    void nojs_root(std::string compiler) {
        if (!ensure_method_get()) {
            return;
        }

        auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
        // find compiler info.
        auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
            [&compiler](cppcms::json::value& v) {
                return v["name"].str() == compiler;
            });
        // error if the compiler is not found
        if (it == compiler_infos.array().end()) {
            response().status(404);
            return;
        }
        content::nojs_root c;
        c.compiler_info = *it;
        render("nojs_root", c);
    }
    void nojs_compile(std::string compiler) {
        if (!ensure_method_post()) {
            return;
        }

        auto json = form_to_json(request().post());
        if (json["compiler"].str() != compiler) {
            response().status(400);
            return;
        }
        auto result = api_compile_internal(json);

        if (json.get("save", false)) {
            std::stringstream ss;
            mapper().map(ss, "nojs-get-permlink", json["compiler"].str(), result["permlink"].str());
            response().status(302);
            response().set_redirect_header(ss.str());
        } else {
            content::nojs_root c;

            auto compiler_infos = get_compiler_infos_or_cache()["compilers"];
            // find compiler info.
            auto it = std::find_if(compiler_infos.array().begin(), compiler_infos.array().end(),
                [&compiler](cppcms::json::value& v) {
                    return v["name"].str() == compiler;
                });
            // error if the compiler is not found
            if (it == compiler_infos.array().end()) {
                response().status(400);
                return;
            }
            c.compiler_info = *it;
            json["compiler_message"] = result["compiler_message"];
            json["program_message"] = result["program_message"];
            c.set_permlink(json);
            render("nojs_root", c);
        }
    }
    void nojs_get_permlink(std::string compiler, std::string permlink_name) {
        if (!ensure_method_get()) {
            return;
        }

        content::nojs_root c;

        permlink pl(service());
        auto result = pl.get_permlink(permlink_name);

        for (auto&& output: result["results"].array()) {
            update_compile_result(result, protocol{output["type"].str(), output["data"].str()});
        }
        result.object().erase("results");

        auto info = result["compiler-info"];
        c.compiler_info = info;
        set_twitter_if_info_exists(c, info, result["code"].str());

        if (!info.is_undefined() && info["name"].str() != compiler) {
            response().status(400);
            return;
        }

        c.set_permlink(std::move(result));

        render("nojs_root", c);
    }

    std::string get_query_string(const std::string& key) {
        const auto& qs = request().query_string();
        auto start_index = qs.find(key + "=");
        if (start_index == std::string::npos) {
            return "";
        }
        start_index += key.length() + 1;

        auto end_index = qs.find("&", start_index);
        if (end_index == std::string::npos) {
            return qs.substr(start_index);
        } else {
            return qs.substr(start_index, end_index - start_index);
        }
    }

    static const int rows_per_page = 10;

    void user(std::string username) {
        permlink pl(service());
        if (!pl.exists_github_user(username)) {
            response().status(404);
            return;
        }

        std::string avatar_url;
        {
            auto author = get_author(username);
            if (author["avatar_url"].type() == cppcms::json::is_string) {
                avatar_url = author["avatar_url"].str() + "&s=40";
            }
        }

        auto auth = authenticate(pl.get_github_access_token(session()["access_token"]));
        auto include_private = !auth.is_undefined() && auth["login"].str() == username;

        int page = 0;
        std::string page_str = get_query_string("p");
        if (!page_str.empty()) {
            page = std::atoi(page_str.c_str());
            if (page < 0) page = 0;
        }

        auto usercode = pl.get_github_usercode(username, include_private, page, rows_per_page);

        content::user c(service());

        if (!auth.is_undefined()) {
            content::user::login_info_t info;
            info.name = auth["login"].str();
            if (!auth["avatar_url"].is_null()) {
                info.avatar_url = auth["avatar_url"].str() + "&s=20";
            }
            c.set_login(info);
        }

        content::user::target_user_info_t uinfo;
        uinfo.username = username;
        uinfo.avatar_url = avatar_url;
        uinfo.usercode = usercode;
        c.set_target(uinfo);

        render("user", c);
    }
};

class kennel_root : public cppcms::application {
    kennel app_;

public:
    kennel_root(cppcms::service &srv) : cppcms::application(srv), app_(srv) {
        auto map_root = srv.settings()["application"]["map_root"].str();
        auto dispatch_root = srv.settings()["application"]["dispatch_root"].str();
        add(app_, "root", map_root + "{1}", dispatch_root + "((/.*)?)", 1);
    }
};

int main(int argc, char** argv) try {
    std::shared_ptr<std::streambuf> logbuf(std::clog.rdbuf(), [](void*){});

    {
        const auto ite = std::find(argv, argv+argc, std::string("--syslog"));
        if (ite != argv+argc) {
            std::clog.rdbuf(new wandbox::syslogstreambuf("kennel2", LOG_PID, LOG_DAEMON, LOG_DEBUG));
            std::rotate(ite, ite+1, argv+argc);
            --argc;
        }
    }

    cppcms::service service(argc, argv);

    permlink pl(service);
    pl.init();

    std::clog << "start get_compiler_infos()" << std::endl;
    auto jsons = kennel::get_compiler_infos(service);
    auto json = kennel::merge_compiler_infos(jsons);
    kennel::default_compiler_infos() = json;
    std::clog << "finish get_compiler_infos()" << std::endl;

    service.applications_pool().mount(
        cppcms::applications_factory<kennel_root>()
    );
    service.run();
} catch (std::exception const &e) {
    std::cerr << e.what() << std::endl;
}

