#ifndef NDJSON_H_INCLUDED
#define NDJSON_H_INCLUDED

#include <string>
#include <sstream>
#include <ios>
#include "libs.h"

class ndjson {
    void send_chunk(const std::string& buf, bool flush) const {
        //std::ostringstream oss;
        //oss << std::hex << buf.size();
        //std::clog << oss.str() << "\r\n" << buf << "\r\n" << std::flush;
        context->response().out() << buf;
        if (flush)
            context->response().out() << std::flush;
    }

public:
    booster::shared_ptr<cppcms::http::context> context;
    ndjson(booster::shared_ptr<cppcms::http::context> context) : context(context) { }

    ndjson() = default;
    ~ndjson() {
      try {
        close();
      } catch (...) {
      }
    }
    //ndjson(const ndjson&) = default;
    ndjson(ndjson&&) = default;
    //ndjson& operator=(const ndjson&) = default;
    ndjson& operator=(ndjson&&) = default;

    void send(const cppcms::json::value& json, bool flush) const {
        std::string json_str = [&json]() {
            std::stringstream ss;
            json.save(ss, cppcms::json::compact);
            return ss.str();
        }();
        json_str += '\n';
        send_chunk(json_str, flush);
    }

    void send_header() const {
        context->response().protocol(1, 1);
        context->response().status(200);
        context->response().content_type("application/x-ndjson");
    }
    void close() const {
        send_chunk("", true);
        context->complete_response();
    }
};

#endif // EVENTSOURCE_H_INCLUDED
