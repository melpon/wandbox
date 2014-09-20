#include <cppcms/application.h>
#include <cppcms/applications_pool.h>
#include <cppcms/url_dispatcher.h>
#include <cppcms/url_mapper.h>
#include <cppcms/service.h>
#include <cppcms/http_context.h>
#include <cppcms/http_response.h>
#include <booster/system_error.h>
#include <iostream>
#include <fstream>
#include "content/root.h"
#include "protocol.h"
#include "eventsource.h"

class kennel : public cppcms::application {
    std::map<std::string, eventsource> eventsources_;
public:
    kennel(cppcms::service &srv) : cppcms::application(srv) {
        dispatcher().assign("/static/([a-zA-Z0-9_\\-/\\.]+\\.(js|css|png))", &kennel::serve_file, this, 1);
        dispatcher().assign("/source/([a-zA-Z0-9]+)", &kennel::connect_source, this, 1);
        dispatcher().assign("/compile/([a-zA-Z0-9]+)", &kennel::compile, this, 1);
    }
    void connect_source(std::string name) {
        auto context = release_context();
        //auto self = booster::intrusive_ptr<kennel>(this);
        context->async_on_peer_reset([this, name]() {
            std::cout << "peer reset: " << name << std::endl;
            this->eventsources_.erase(name);
        });
        eventsources_[name] = eventsource(context);
    }
    void compile(std::string name) {
        for (auto&& x: eventsources_)
            std::cout << x.first << std::endl;
        auto it = eventsources_.find(name);
        if (it == eventsources_.end())
            return;

        std::vector<protocol> protos = {
            protocol{"Control", "compiler=gcc-4.8.1"},
            protocol{"StdIn", ""},
            protocol{"CompilerOptionRaw", ""},
            protocol{"RuntimeOptionRaw", ""},
            protocol{"Source", "#include<iostream>\nint main() { std::cout << \"hoge-\" << std::endl; }\n"},
            protocol{"CompilerOption", ""},
            protocol{"Control", "run"},
        };
        it->second.send_header();
        auto self = booster::intrusive_ptr<kennel>(this);
        send_command(service().get_io_service(), protos, [self, name](const booster::system::error_code& e, const protocol& proto) {
            if (e)
                return (void)(std::cout << e.message() << std::endl);
            auto it = self->eventsources_.find(name);
            if (it == self->eventsources_.end())
                return;
            it->second.send_data(proto.command + ": " + proto.contents, true);
            std::cout << proto.command << ": " << proto.contents << std::endl;
        });
    }
    //virtual void main(std::string url) {
    //    //content::root c;
    //    //c.title = "[Wandbox]三へ( へ՞ਊ ՞)へ ﾊｯﾊｯ";
    //    //render("root", c);
    //    //f(this->service());
    //    //response().out() << "HTTP/1.1 200 OK\n";
    //    eventsource es(release_context());
    //    es.send_header();
    //    es.send_data("test", true);
    //    es.send_data("test\n", true);
    //    es.send_data("test\n\n", true);
    //    es.close();
    //}
    void serve_file(std::string file_name) {
        std::cout << file_name << std::endl;
        std::ifstream f(("../kennel/static/" + file_name).c_str());
        if (!f) {
            response().status(404);
        } else {
            response().content_type("application/octet-stream");
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

