#ifndef EVENTSOURCE_H_INCLUDED
#define EVENTSOURCE_H_INCLUDED

#include <string>
#include <sstream>
#include <ios>
#include "libs.h"

class eventsource {
    void send_chunk(const std::string& buf, bool flush) const {
        std::ostringstream oss;
        oss << std::hex << buf.size();
        //std::clog << oss.str() << "\r\n" << buf << "\r\n" << std::flush;
        context->response().out() << oss.str() << "\r\n" << buf << "\r\n";
        if (flush)
            context->response().out() << std::flush;
    }

public:
    booster::shared_ptr<cppcms::http::context> context;
    eventsource(booster::shared_ptr<cppcms::http::context> context) : context(context) { }

    eventsource() = default;
    ~eventsource() {
        try {
            close();
        } catch (...) { }
    }
    //eventsource(const eventsource&) = default;
    eventsource(eventsource&&) = default;
    //eventsource& operator=(const eventsource&) = default;
    eventsource& operator=(eventsource&&) = default;

    void send(const std::string& name, const std::string& contents, bool flush) const {
        std::string buf;
        buf += name;
        buf += ": ";
        for (std::size_t i = 0; i < contents.size(); i++) {
            auto c = contents[i];
            buf += c;
            if (c == '\n') {
                buf += name;
                buf += ": ";
            } else if (c == '\r') {
                auto c2 = contents[i + 1];
                if (c2 == '\n') {
                    buf += c2;
                    i += 1;
                }
                buf += name;
                buf += ": ";
            }
        }
        buf += '\n';
        if (flush) {
            buf += '\n';
        }
        send_chunk(buf, flush);
    }

    void send_header() const {
        context->response().protocol(1, 1);
        context->response().status(200);
        context->response().content_type("text/event-stream");
        context->response().transfer_encoding("chunked");
    }
    void send_id(const std::string& id, bool flush) const {
        send("id", id, flush);
    }
    void send_event(const std::string& event, bool flush) const {
        send("event", event, flush);
    }
    void send_data(const std::string& data, bool flush) const {
        send("data", data, flush);
    }
    void send_retry(int milliseconds, bool flush) const {
        send("retry", std::to_string(milliseconds), flush);
    }
    void send_comment(std::string comment, bool flush) const {
        send("", comment, flush);
    }
    void close() const {
        send_chunk("", true);
        context->complete_response();
    }
};

#endif // EVENTSOURCE_H_INCLUDED
