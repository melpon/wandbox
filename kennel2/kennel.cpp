#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <cppcms/service.h>
#include <cppcms/http_context.h>
#include <cppcms/http_response.h>
#include <cppcms/json.h>
#include <cppcms/serialization.h>
#include <booster/system_error.h>
#include <booster/posix_time.h>
#include <booster/aio/deadline_timer.h>
#include <iostream>
#include <fstream>
#include <random>
#include "content/root.h"
#include "protocol.h"
#include "eventsource.h"
#include "permlink.h"

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
    booster::aio::deadline_timer update_timer;
public:
    kennel(cppcms::service &srv) : cppcms::application(srv) {
        dispatcher().assign("/static/([a-zA-Z0-9_\\-/\\.]+\\.(js|css|png|gif))", &kennel::serve_file, this, 1, 2);
        mapper().assign("static", "/static");

        dispatcher().assign("/compile/?", &kennel::compile, this);
        mapper().assign("compile", "/compile");

        dispatcher().assign("/permlink/?", &kennel::post_permlink, this);
        mapper().assign("permlink", "/permlink");

        dispatcher().assign("/permlink/([a-zA-Z0-9]+)/?", &kennel::get_permlink, this, 1);

        dispatcher().assign("/api/list.json", &kennel::api_list, this);
        dispatcher().assign("/api/compile.json", &kennel::api_compile, this);
        dispatcher().assign("/api/permlink/([a-zA-Z0-9]+)/?", &kennel::api_permlink, this, 1);

        dispatcher().assign("/?", &kennel::root, this);
        mapper().assign("root", "");

        mapper().root(srv.settings()["application"]["root"].str());
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
                cache().store_data("compiler_infos", json, 10);
                cache().store_data("compiler_infos_persist", json, -1);
            } else {
                cache().store_data("compiler_infos", json, 10);

                update_timer.set_io_service(service().get_io_service());
                update_timer.expires_from_now(booster::ptime::seconds(1));
                booster::intrusive_ptr<kennel> self(this);
                update_timer.async_wait([self](const booster::system::error_code& e) {
                    auto json = self->get_compiler_infos();
                    self->cache().store_data("compiler_infos", json, 10);
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
        send_command(service().get_io_service(), protos, [&json](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::cout << e.message() << std::endl);
            json = proto.contents;
        }, 1);
        std::stringstream ss(json);
        cppcms::json::value value;
        value.load(ss, true, nullptr);
        //value.save(std::cout, cppcms::json::readable);
        return value;
    }

    void root() {
        content::root c;
        c.compiler_infos = get_compiler_infos_or_cache();
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
            protocol{"Control", "run"},
        };
        return protos;
    }
    void compile() {
        if (request().request_method() != "POST") {
            response().status(404);
            return;
        }
        auto value = json_post_data();
        auto protos = make_protocols(value);

        auto es = eventsource(release_context());
        es.send_header();
        send_command_async(service().get_io_service(), protos, [es](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::cout << e.message() << std::endl);
            es.send_data(proto.command + ":" + proto.contents, true);
            std::cout << proto.command << ":" << proto.contents << std::endl;
        });
    }
    static std::string make_random_name() {
        std::string name;
        std::random_device seed_gen;
        std::mt19937 engine(seed_gen());
        std::uniform_int_distribution<> dist(0, 127);
        while (name.size() < 16) {
            auto c = (char)dist(engine);
            if ('0' <= c && c <= '9' ||
                'a' <= c && c <= 'z' ||
                'A' <= c && c <= 'Z') {
                name.push_back(c);
            }
        }
        return name;
    }
    void post_permlink() {
        if (request().request_method() != "POST") {
            response().status(404);
            return;
        }
        auto value = json_post_data();
        permlink pl(service());
        std::string permlink_name = make_random_name();
        pl.make_permlink(permlink_name, value);

        response().content_type("application/json");
        cppcms::json::value result;
        result["success"] = true;
        result["link"] = permlink_name;
        result.save(response().out(), cppcms::json::compact);
    }
    void get_permlink(std::string permlink_name) {
        content::root c;
        c.compiler_infos = get_compiler_infos_or_cache();

        permlink pl(service());
        auto result = pl.get_permlink(permlink_name);
        std::stringstream ss;
        result.save(ss, cppcms::json::compact);
        c.using_permlink = true;
        c.permlink = ss.str();

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
        response().content_type("application/json");
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
        if (request().request_method() != "POST") {
            response().status(404);
            return;
        }

        auto value = json_post_data();
        auto save = value.get("save", false);
        auto protos = make_protocols(value);

        cppcms::json::value result;
        cppcms::json::value outputs;
        outputs.array({});
        send_command(service().get_io_service(), protos, [this, &result, &outputs, save](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::cout << e.message() << std::endl);

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
            pl.make_permlink(permlink_name, value);
            result["permlink"] = permlink_name;

            auto settings = service().settings()["application"];
            auto scheme = settings["scheme"].str();
            auto domain = settings["domain"].str();
            auto root = settings["root"].str();
            result["url"] = scheme + "://" + domain + root + "/permlink/" + permlink_name;
        }
        response().content_type("application/json");
        result.save(response().out(), cppcms::json::readable);
    }
    void api_permlink(std::string permlink_name) {
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
        result.save(response().out(), cppcms::json::readable);
    }
};

int main(int argc, char** argv) try {
    cppcms::service service(argc, argv);

    permlink pl(service);
    pl.init();

    service.applications_pool().mount(
        cppcms::applications_factory<kennel>()
    );
    service.run();
} catch (std::exception const &e) {
    std::cerr << e.what() << std::endl;
}

