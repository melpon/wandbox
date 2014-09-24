#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <cppcms/service.h>
#include <cppcms/http_context.h>
#include <cppcms/http_response.h>
#include <cppcms/json.h>
#include <booster/system_error.h>
#include <iostream>
#include <fstream>
#include "content/root.h"
#include "protocol.h"
#include "eventsource.h"

class kennel : public cppcms::application {
public:
    kennel(cppcms::service &srv) : cppcms::application(srv) {
        dispatcher().assign("/static/([a-zA-Z0-9_\\-/\\.]+\\.(js|css|png|gif))", &kennel::serve_file, this, 1, 2);
        mapper().assign("static", "/static");

        dispatcher().assign("/compile/?", &kennel::compile, this);
        mapper().assign("compile", "/compile");

        dispatcher().assign("/?", &kennel::root, this);
        mapper().assign("root", "");

        mapper().root("/wandbox/cppcms");
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
        });
        std::stringstream ss(json);
        cppcms::json::value value;
        value.load(ss, true, nullptr);
        //value.save(std::cout, cppcms::json::readable);
        return value;
    }

    void root() {
        content::root c;
        c.compiler_infos = get_compiler_infos();
        render("root", c);
    }
    void compile() {
        if (request().request_method() != "POST") {
            response().status(404);
            return;
        }
        auto p = request().raw_post_data();
        std::stringbuf sb(std::ios_base::in);
        sb.pubsetbuf(static_cast<char*>(p.first), p.second);
        std::istream is(&sb);
        cppcms::json::value value;
        value.load(is, true, nullptr);
        std::vector<protocol> protos = {
            protocol{"Control", "compiler=" + value["compiler"].str()},
            protocol{"StdIn", value.get("stdin", "")},
            protocol{"CompilerOptionRaw", value.get("compiler-option-raw", "")},
            protocol{"RuntimeOptionRaw", value.get("runtime-option-raw", "")},
            protocol{"Source", value["code"].str()},
            protocol{"CompilerOption", value.get("options", "")},
            protocol{"Control", "run"},
        };

        auto es = eventsource(release_context());
        es.send_header();
        send_command_async(service().get_io_service(), protos, [es](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::cout << e.message() << std::endl);
            es.send_data(proto.command + ":" + proto.contents, true);
            std::cout << proto.command << ":" << proto.contents << std::endl;
        });
    }
    void serve_file(std::string file_name, std::string ext) {
        std::cout << file_name << std::endl;
        std::ifstream f(("../kennel/static/" + file_name).c_str());
        if (!f)
            f.open(("static/" + file_name).c_str());

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
};

int main(int argc, char** argv) try {
    cppcms::service service(argc, argv);
    service.applications_pool().mount(
        cppcms::applications_factory<kennel>()
    );
    service.run();
} catch (std::exception const &e) {
    std::cerr << e.what() << std::endl;
}

