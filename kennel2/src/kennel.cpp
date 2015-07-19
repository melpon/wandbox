#include <iostream>
#include <fstream>
#include <random>
#include <algorithm>
#include "libs.h"
#include "root.h"
#include "nojs_root.h"
#include "protocol.h"
#include "eventsource.h"
#include "permlink.h"
#include "../../cattleshed/src/syslogstream.cc"

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

        dispatcher().assign("/api/list.json", &kennel::api_list, this);
        dispatcher().assign("/api/compile.json", &kennel::api_compile, this);
        dispatcher().assign("/api/permlink/([a-zA-Z0-9]+)/?", &kennel::api_permlink, this, 1);

        dispatcher().assign("/nojs/(.+)/compile/?", &kennel::nojs_compile, this, 1);
        mapper().assign("nojs-compile", "/nojs/{1}/compile");
        dispatcher().assign("/nojs/([a-zA-Z0-9_\\-\\.]+)/?", &kennel::nojs_root, this, 1);
        mapper().assign("nojs-root", "/nojs/{1}");
        dispatcher().assign("/nojs/?", &kennel::nojs_list, this);
        mapper().assign("nojs-list", "/nojs");

        dispatcher().assign("/?", &kennel::root, this);
        if (srv.settings()["application"]["map_root"].str().empty()) {
            mapper().assign("root", "/");
        } else {
            mapper().assign("root", "");
        }
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
                json = get_compiler_infos();
                cache().store_data("compiler_infos", json, 600);
                cache().store_data("compiler_infos_persist", json, -1);
            } else {
                cache().store_data("compiler_infos", json, 600);

                booster::intrusive_ptr<kennel> self(this);
                get_compiler_infos_async([self](cppcms::json::value& json) {
                    self->cache().store_data("compiler_infos", json, 600);
                    self->cache().store_data("compiler_infos_persist", json, -1);
                });
            }
        }
        return json;
    }
    cppcms::json::value get_compiler_infos() {
        std::vector<protocol> protos = {
            protocol{"Version", ""},
        };
        std::string json;
        send_command(service(), protos, [&json](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);
            json = proto.contents;
        }, 1);
        std::stringstream ss(json);
        cppcms::json::value value;
        value.load(ss, true, nullptr);
        //value.save(std::clog, cppcms::json::readable);
        return value;
    }
    template<class F>
    void get_compiler_infos_async(const F& callback) {
        std::vector<protocol> protos = {
            protocol{"Version", ""},
        };
        send_command_async(service(), protos, [callback](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);
            std::stringstream ss(proto.contents);
            cppcms::json::value value;
            value.load(ss, true, nullptr);
            //value.save(std::clog, cppcms::json::readable);
            callback(value);
        }, 1);
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
            return false;
        } else {
            response().status(405);
            response().allow("OPTIONS, POST");
            return false;
        }
    }

    void root() {
        if (!ensure_method_get()) {
            return;
        }

        content::root c;
        c.set_compiler_infos(get_compiler_infos_or_cache());
        render("root", c);
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
        return result;
    }
    void compile() {
        if (!ensure_method_post()) {
            return;
        }

        auto value = json_post_data();
        auto protos = make_protocols(value);

        auto es = eventsource(release_context());
        es.send_header();
        send_command_async(service(), protos, [es](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);
            es.send_data(proto.command + ":" + proto.contents, true);
            //std::clog << proto.command << ":" << proto.contents << std::endl;
        });
    }
    static std::string make_random_name() {
        std::string name;
        std::random_device seed_gen;
        std::mt19937 engine(seed_gen());
        std::uniform_int_distribution<> dist(0, 127);
        while (name.size() < 16) {
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

        auto compiler_infos = get_compiler_infos_or_cache();
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

        permlink pl(service());
        std::string permlink_name = make_random_name();
        pl.make_permlink(permlink_name, value, *it);

        response().content_type("application/json");
        cppcms::json::value result;
        result["success"] = true;
        result["link"] = permlink_name;
        result.save(response().out(), cppcms::json::compact);
    }
    void get_permlink(std::string permlink_name) {
        if (!ensure_method_get()) {
            return;
        }

        content::root c;
        c.set_compiler_infos(get_compiler_infos_or_cache());

        permlink pl(service());
        auto result = pl.get_permlink(permlink_name);
        std::stringstream ss;
        result.save(ss, cppcms::json::compact);
        c.using_permlink = true;
        c.permlink = ss.str();

        auto info = result["compiler-info"];
        if (!info.is_undefined()) {
            std::string title = "[" + info["language"].str() + "] " + info["display-name"].str() + " " + info["version"].str();
            std::string description = result["code"].str();
            c.set_twitter(std::move(title), std::move(description));
        }

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

    void api_list() {
        if (!ensure_method_get()) {
            return;
        }

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        get_compiler_infos_or_cache().save(response().out(), cppcms::json::compact);
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

        auto value = json_post_data();
        api_compile_internal(json_post_data());
    }
    void api_compile_internal(cppcms::json::value value) {
        auto compiler = value["compiler"].str();
        auto save = value.get("save", false);
        auto protos = make_protocols(value);
        auto compiler_infos = get_compiler_infos_or_cache();
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

        cppcms::json::value result;
        cppcms::json::value outputs;
        outputs.array({});
        send_command(service(), protos, [this, &result, &outputs, save](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::clog << e.message() << std::endl);

            update_compile_result(result, proto);

            if (save) {
                cppcms::json::value v;
                v["type"] = proto.command;
                v["output"] = proto.contents;
                outputs.array().push_back(v);
            }
        });

        if (save) {
            permlink pl(service());
            std::string permlink_name = make_random_name();
            value["outputs"] = outputs;
            pl.make_permlink(permlink_name, value, *it);
            result["permlink"] = permlink_name;

            auto settings = service().settings()["application"];
            auto scheme = settings["scheme"].str();
            auto domain = settings["domain"].str();
            auto root = settings["map_root"].str();
            result["url"] = scheme + "://" + domain + root + "/permlink/" + permlink_name;
        }
        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        result.save(response().out(), cppcms::json::readable);
    }
    void api_permlink(std::string permlink_name) {
        if (!ensure_method_get()) {
            return;
        }

        permlink pl(service());
        auto value = pl.get_permlink(permlink_name);
        cppcms::json::value outputs;
        for (auto&& output: value["outputs"].array()) {
            update_compile_result(outputs, protocol{output["type"].str(), output["output"].str()});
        }
        value.object().erase("outputs");

        cppcms::json::value result;
        result["parameter"] = value;
        result["result"] = outputs;

        response().content_type("application/json");
        response().set_header("Access-Control-Allow-Origin", "*");
        result.save(response().out(), cppcms::json::readable);
    }

    void nojs_list() {
        if (!ensure_method_get()) {
            return;
        }

        content::root c;
        c.set_compiler_infos(get_compiler_infos_or_cache());
        render("nojs_list", c);
    }

    void nojs_root(std::string compiler) {
        if (!ensure_method_get()) {
            return;
        }

        auto compiler_infos = get_compiler_infos_or_cache();
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
        api_compile_internal(json);
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

    service.applications_pool().mount(
        cppcms::applications_factory<kennel_root>()
    );
    service.run();
} catch (std::exception const &e) {
    std::cerr << e.what() << std::endl;
}

