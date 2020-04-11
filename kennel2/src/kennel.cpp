#include <algorithm>
#include <fstream>
#include <future>
#include <iostream>
#include <random>
#include <sstream>
#include "../../cattleshed/src/syslogstream.cc"
#include "cattleshed_client.h"
#include "eventsource.h"
#include "http_client.h"
#include "libs.h"
#include "ndjson.h"
#include "nojs_root.h"
#include "permlink.h"
#include "protocol.h"
#include "root.h"
#include "user.h"

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
  kennel(cppcms::service& srv) : cppcms::application(srv) {
    // static file serving is for debugging
    // use Apache, nginx and so on in production
    dispatcher().assign(
        "/static/(([a-zA-Z0-9_\\-]+/"
        ")*+([a-zA-Z0-9_\\-]+)\\.(js|css|png|gif))",
        &kennel::serve_file, this, 1, 4);
    dispatcher().assign("/static/(js/jquery.cookie.(js))", &kennel::serve_file,
                        this, 1, 2);
    dispatcher().assign("/static/(js/jquery.url.(js))", &kennel::serve_file,
                        this, 1, 2);
    mapper().assign("static", "/static");

    dispatcher().assign("/compile/?", &kennel::compile, this);
    mapper().assign("compile", "/compile");

    dispatcher().assign("/permlink/?", &kennel::post_permlink, this);
    mapper().assign("permlink", "/permlink");

    dispatcher().assign("/permlink/([a-zA-Z0-9]+)/?", &kennel::get_permlink,
                        this, 1);
    mapper().assign("get-permlink", "/permlink/{1}");

    dispatcher().assign("/login/github/callback", &kennel::get_github_callback,
                        this);

    dispatcher().assign("/api/user.json", &kennel::api_user, this);
    dispatcher().assign("/api/list.json", &kennel::api_list, this);
    dispatcher().assign("/api/compile.json", &kennel::api_compile, this);
    dispatcher().assign("/api/compile.ndjson", &kennel::api_compile_ndjson,
                        this);
    dispatcher().assign("/api/permlink/?", &kennel::api_post_permlink, this);
    dispatcher().assign("/api/permlink/([a-zA-Z0-9]+)/?", &kennel::api_permlink,
                        this, 1);
    dispatcher().assign("/api/template/(.+?)/?", &kennel::api_template, this,
                        1);
    mapper().assign("api-template", "/api/template/");

    dispatcher().assign("/nojs/(.+?)/compile/?", &kennel::nojs_compile, this,
                        1);
    mapper().assign("nojs-compile", "/nojs/{1}/compile");
    dispatcher().assign("/nojs/(.+?)/permlink/([a-zA-Z0-9]+)/?",
                        &kennel::nojs_get_permlink, this, 1, 2);
    mapper().assign("nojs-get-permlink", "/nojs/{1}/permlink/{2}");
    dispatcher().assign("/nojs/([a-zA-Z0-9_\\-\\.]+)/?", &kennel::nojs_root,
                        this, 1);
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
  static CattleshedClientManager* get_cattleshed_client_manager(
      cppcms::service& srv, int n) {
    static std::map<int, std::unique_ptr<CattleshedClientManager>> cmmap;
    auto it = cmmap.find(n);
    if (it != cmmap.end()) {
      return it->second.get();
    }

    auto host = srv.settings()["application"]["cattleshed"][n]["host"].str();
    //auto port =
    //    (int)srv.settings()["application"]["cattleshed"][n]["port"].number();
    auto port = 50051;
    auto channel = grpc::CreateChannel(host + ":" + std::to_string(port),
                                       grpc::InsecureChannelCredentials());
    SPDLOG_INFO("Create gRPC channel to {}", host + ":" + std::to_string(port));
    std::unique_ptr<CattleshedClientManager> cm(
        new CattleshedClientManager(channel, 1));
    auto p = cm.get();
    cmmap.insert(std::make_pair(n, std::move(cm)));
    return p;
  }

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

        auto context = get_context();
        get_compiler_infos_async(
            service(),
            [context](std::vector<cattleshed::GetVersionResponse> responses) {
              std::vector<cppcms::json::value> jsons;
              for (const auto& resp : responses) {
                jsons.push_back(kennel::version_response_to_json(resp));
              }
              auto json = kennel::merge_compiler_infos(jsons);
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
  static cppcms::json::value merge_compiler_infos(
      const std::vector<cppcms::json::value>& infos) {
    std::vector<cppcms::json::value> compilers;
    cppcms::json::object templates;
    for (auto i = 0; i < static_cast<int>(infos.size()); i++) {
      for (const auto& info : infos[i]["compilers"].array()) {
        auto name = info["name"];
        auto it = std::find_if(
            compilers.begin(), compilers.end(),
            [name](cppcms::json::value& v) { return v["name"] == name; });

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
      for (const auto& info : infos[i]["templates"].object()) {
        templates[info.first] = info.second;
      }
    }
    cppcms::json::value r;
    r.set("compilers", compilers);
    r.set("templates", templates);
    return r;
  }

  // cattleshed::GetVersionResponse を頑張って JSON にする
  static cppcms::json::value version_response_to_json(
      const cattleshed::GetVersionResponse& resp) {
    cppcms::json::value compilers = cppcms::json::array();
    for (int i = 0; i < resp.compiler_info_size(); i++) {
      cppcms::json::value info;

      const cattleshed::CompilerInfo& c = resp.compiler_info(i);
      info["name"] = c.name();
      info["version"] = c.version();
      info["language"] = c.language();
      info["display-name"] = c.display_name();
      info["templates"] = cppcms::json::array();
      for (int j = 0; j < c.templates_size(); j++) {
        info["templates"].array().push_back(c.templates(j));
      }
      info["compiler-option-raw"] = c.compiler_option_raw();
      info["runtime-option-raw"] = c.runtime_option_raw();
      info["display-compile-command"] = c.display_compile_command();
      cppcms::json::value switches = cppcms::json::array();
      for (int j = 0; j < c.switches_size(); j++) {
        cppcms::json::value sw;

        const cattleshed::Switch& s = c.switches(j);
        if (s.data_case() == cattleshed::Switch::kSingle) {
          const cattleshed::SingleSwitch& ss = s.single();
          sw["type"] = "single";
          sw["name"] = ss.name();
          sw["default"] = ss.default_value();
          sw["display-name"] = ss.display_name();
          sw["display-flags"] = ss.display_flags();
        } else {
          const cattleshed::SelectSwitch& ss = s.select();
          sw["type"] = "select";
          sw["default"] = ss.default_value();
          for (int k = 0; k < ss.options_size(); k++) {
            cppcms::json::value option;
            const cattleshed::SelectSwitchOption& opt = ss.options(k);
            option["name"] = opt.name();
            option["display-name"] = opt.display_name();
            option["display-flags"] = opt.display_flags();
            sw["options"] = cppcms::json::array();
            sw["options"].array().push_back(option);
          }
        }

        switches.array().push_back(sw);
      }
      info["switches"] = switches;

      compilers.array().push_back(info);
    }

    cppcms::json::value templates = cppcms::json::object();
    for (int i = 0; i < resp.templates_size(); i++) {
      cppcms::json::value tmpl;
      {
        const cattleshed::Template& t = resp.templates(i);
        tmpl["name"] = t.name();
        tmpl["code"] = t.default_source();
        if (t.sources_size() != 0) {
          for (int j = 0; j < t.sources_size(); j++) {
            cppcms::json::value code;
            code["file"] = t.sources(j).file_name();
            code["code"] = t.sources(j).source();
            tmpl["codes"].array().push_back(code);
          }
        }
        if (!t.stdin().empty()) {
          tmpl["stdin"] = t.stdin();
        }
        if (!t.compiler_options().empty()) {
          tmpl["options"] = t.compiler_options();
        }
        if (!t.compiler_option_raw().empty()) {
          tmpl["compiler_option_raw"] = t.compiler_option_raw();
        }
        if (!t.runtime_option_raw().empty()) {
          tmpl["runtime_option_raw"] = t.runtime_option_raw();
        }
      }
      templates.object()[tmpl["name"].str()] = tmpl;
    }

    cppcms::json::value r;
    r["compilers"] = compilers;
    r["templates"] = templates;
    return r;
  }

  static std::vector<cattleshed::GetVersionResponse> get_compiler_infos(
      cppcms::service& service) {
    std::promise<std::vector<cattleshed::GetVersionResponse>> promise;
    std::future<std::vector<cattleshed::GetVersionResponse>> future =
        promise.get_future();
    get_compiler_infos_async(
        service, [&promise](std::vector<cattleshed::GetVersionResponse> resp) {
          promise.set_value(std::move(resp));
        });
    auto status = future.wait_for(std::chrono::seconds(60));
    if (status == std::future_status::timeout) {
      throw std::exception();
    }
    return future.get();
  }

 private:
  static void get_compiler_infos_async(
      cppcms::service& service,
      std::function<void(std::vector<cattleshed::GetVersionResponse>)>
          callback) {
    auto results =
        std::make_shared<std::vector<cattleshed::GetVersionResponse>>();
    auto count = std::make_shared<int>(0);
    auto size = service.settings()["application"]["cattleshed"].array().size();
    for (auto i = 0; i < static_cast<int>(size); i++) {
      auto cm = get_cattleshed_client_manager(service, i);
      auto client = cm->CreateGetVersionClient();
      client->SetOnResponse(
          [callback, results, client, count, size](
              cattleshed::GetVersionResponse resp, grpc::Status status) {
            if (status.ok()) {
              results->push_back(resp);
            }
            *count += 1;
            if (*count == size) {
              callback(std::move(*results));
            }
          });
      cattleshed::GetVersionRequest req;
      client->Request(req);
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

    static std::vector<protocol> make_protocols(
        const cppcms::json::value& value) {
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
        for (const auto& code : codes.array()) {
          protos.push_back(protocol{"SourceFileName", code["file"].str()});
          protos.push_back(protocol{"Source", code["code"].str()});
        }
      }
      protos.push_back(protocol{"Control", "run"});
      return protos;
    }

    static cattleshed::RunJobRequest make_run_job_request(
        const cppcms::json::value& value) {
      cattleshed::RunJobRequest request;
      auto start = request.mutable_start();
      start->set_compiler(value["compiler"].str());
      start->set_stdin(value.get("stdin", ""));
      start->set_compiler_option_raw(value.get("compiler-option-raw", ""));
      start->set_runtime_option_raw(value.get("compiler-option-raw", ""));
      start->set_default_source(value["code"].str());
      start->set_compiler_options(value.get("options", ""));
      const auto& codes = value.find("codes");
      if (!codes.is_undefined()) {
        for (const auto& code : codes.array()) {
          auto source = start->add_sources();
          source->set_file_name(code["file"].str());
          source->set_source(code["code"].str());
        }
      }
      return request;
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
      auto it = std::find_if(compiler_infos.array().begin(),
                             compiler_infos.array().end(),
                             [&compiler](cppcms::json::value& v) {
                               return v["name"].str() == compiler;
                             });
      // error if the compiler is not found
      if (it == compiler_infos.array().end()) {
        throw std::exception();
      }
      auto index = static_cast<std::size_t>((*it)["provider"].number());

      auto es =
          booster::shared_ptr<eventsource>(new eventsource(release_context()));
      es->send_header();

      auto request = make_run_job_request(value);

      SPDLOG_DEBUG("[client] send RunJobRequest: {}", request.DebugString());

      auto cm = get_cattleshed_client_manager(service(), index);
      auto client = cm->CreateRunJobClient();
      client->SetOnRead([client, es](cattleshed::RunJobResponse resp) {
        SPDLOG_DEBUG("[client] OnRead: {}", resp.DebugString());

        es->send_data(response_type_to_string(resp.type()) + ":" + resp.data(),
                      true);
      });
      client->SetOnReadDone([client](grpc::Status status) {
        SPDLOG_DEBUG("[client] OnReadDone");
        client->Shutdown();
      });
      client->Connect();
      client->Write(std::move(request));
      client->WritesDone();
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
    static void update_compile_result(cppcms::json::value& result,
                                      const cattleshed::RunJobResponse& resp) {
      static auto append = [](cppcms::json::value& v, const std::string& str) {
        if (v.is_undefined())
          v = str;
        else
          v.str() += str;
      };
      if (resp.type() == cattleshed::RunJobResponse::CONTROL) {
      } else if (resp.type() == cattleshed::RunJobResponse::COMPILER_STDOUT) {
        append(result["compiler_output"], resp.data());
        append(result["compiler_message"], resp.data());
      } else if (resp.type() == cattleshed::RunJobResponse::COMPILER_STDERR) {
        append(result["compiler_error"], resp.data());
        append(result["compiler_message"], resp.data());
      } else if (resp.type() == cattleshed::RunJobResponse::STDOUT) {
        append(result["program_output"], resp.data());
        append(result["program_message"], resp.data());
      } else if (resp.type() == cattleshed::RunJobResponse::STDERR) {
        append(result["program_error"], resp.data());
        append(result["program_message"], resp.data());
      } else if (resp.type() == cattleshed::RunJobResponse::EXIT_CODE) {
        append(result["status"], resp.data());
      } else if (resp.type() == cattleshed::RunJobResponse::SIGNAL) {
        append(result["signal"], resp.data());
      } else {
        //append(result["error"], resp.data());
      }
    }
    static std::string response_type_to_string(
        cattleshed::RunJobResponse::Type type) {
      switch (type) {
        case cattleshed::RunJobResponse::CONTROL:
          return "Control";
        case cattleshed::RunJobResponse::COMPILER_STDOUT:
          return "CompilerMessageS";
        case cattleshed::RunJobResponse::COMPILER_STDERR:
          return "CompilerMessageE";
        case cattleshed::RunJobResponse::STDOUT:
          return "StdOut";
        case cattleshed::RunJobResponse::STDERR:
          return "StdErr";
        case cattleshed::RunJobResponse::EXIT_CODE:
          return "ExitCode";
        case cattleshed::RunJobResponse::SIGNAL:
          return "Signal";
        default:
          return "";
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
        auto request = make_run_job_request(value);
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

        std::promise<void> promise;
        std::future<void> future = promise.get_future();

        cppcms::json::value result;
        cppcms::json::value outputs;
        outputs.array({});

        SPDLOG_DEBUG("[client] send RunJobRequest: {}", request.DebugString());

        auto cm = get_cattleshed_client_manager(service(), index);
        auto client = cm->CreateRunJobClient();
        client->SetOnRead(
            [&result, &outputs, save](cattleshed::RunJobResponse resp) {
              update_compile_result(result, resp);

              if (save) {
                cppcms::json::value v;
                v["type"] = response_type_to_string(resp.type());
                v["data"] = resp.data();
                outputs.array().push_back(v);
              }
            });
        client->SetOnReadDone(
            [&promise](grpc::Status status) { promise.set_value(); });
        client->Connect();
        client->Write(std::move(request));
        client->WritesDone();

        // send_run_job が終わるまで待つ
        auto status = future.wait_for(std::chrono::seconds(60));
        if (status == std::future_status::timeout) {
          throw std::exception();
        }

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
      auto it = std::find_if(compiler_infos.array().begin(),
                             compiler_infos.array().end(),
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

      auto request = make_run_job_request(value);

      SPDLOG_DEBUG("[client] send RunJobRequest: {}", request.DebugString());

      auto cm = get_cattleshed_client_manager(service(), index);
      auto client = cm->CreateRunJobClient();
      client->SetOnRead([nd](cattleshed::RunJobResponse resp) {
        SPDLOG_DEBUG("[client] OnRead: {}", resp.DebugString());

        cppcms::json::value v;
        v["type"] = response_type_to_string(resp.type());
        v["data"] = resp.data();
        nd->send(v, true);
      });
      client->SetOnReadDone([client](grpc::Status status) {
        SPDLOG_DEBUG("[client] OnReadDone");
        client->Shutdown();
      });
      client->Connect();
      client->Write(std::move(request));
      client->WritesDone();
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
  spdlog::set_level(spdlog::level::trace);

  std::shared_ptr<std::streambuf> logbuf(std::clog.rdbuf(), [](void*) {});

  {
    const auto ite = std::find(argv, argv + argc, std::string("--syslog"));
    if (ite != argv + argc) {
      std::clog.rdbuf(new wandbox::syslogstreambuf("kennel2", LOG_PID,
                                                   LOG_DAEMON, LOG_DEBUG));
      std::rotate(ite, ite + 1, argv + argc);
      --argc;
    }
  }

  cppcms::service service(argc, argv);

  permlink pl(service);
  pl.init();

  SPDLOG_INFO("start get_compiler_infos()");
  auto responses = kennel::get_compiler_infos(service);
  std::vector<cppcms::json::value> jsons;
  for (const auto& resp : responses) {
    jsons.push_back(kennel::version_response_to_json(resp));
  }
  auto json = kennel::merge_compiler_infos(jsons);

  SPDLOG_INFO("finish get_compiler_infos()");
  {
    std::stringstream ss;
    json.save(ss, cppcms::json::readable);
    SPDLOG_TRACE("contents: {}", ss.str());
  }
  kennel::default_compiler_infos() = json;

  service.applications_pool().mount(
      cppcms::applications_factory<kennel_root>());
  service.run();
} catch (std::exception const& e) {
  std::cerr << e.what() << std::endl;
}

