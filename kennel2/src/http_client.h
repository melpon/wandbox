#ifndef HTTP_CLIENT_H_INCLUDED
#define HTTP_CLIENT_H_INCLUDED

#include <string>
#include <vector>
#include <curl/curl.h>

struct http_client {
    struct response {
        long status_code;
        std::string body;
        // implements if needed
        // std::map<std::string, std::string> headers;
    };

    static size_t receive_callback(void* contents, size_t size, size_t nmemb, void* userp) {
        std::string& resp = *(std::string*)userp;

        std::size_t realsize = size * nmemb;
        resp.append((const char*)contents, realsize);

        return realsize;
    }

    static http_client::response post(std::string url, std::vector<std::string> headers, std::string body) {
        struct context {
            CURL* curl;
            curl_slist* chunk;
            context() : curl(nullptr), chunk(nullptr) {}
            ~context() {
                if (curl != nullptr) curl_easy_cleanup(curl);
                if (chunk != nullptr) curl_slist_free_all(chunk);
            }
        } ctx;

        ctx.curl = curl_easy_init();

        // set url
        curl_easy_setopt(ctx.curl, CURLOPT_URL, url.c_str());

        // set body
        curl_easy_setopt(ctx.curl, CURLOPT_POSTFIELDS, body.c_str());
        curl_easy_setopt(ctx.curl, CURLOPT_POSTFIELDSIZE, body.size());

        // set header
        for (auto&& header: headers) {
            ctx.chunk = curl_slist_append(ctx.chunk, header.c_str());
        }
        curl_easy_setopt(ctx.curl, CURLOPT_HTTPHEADER, ctx.chunk);

        // set response callback
        std::string resp_body;
        curl_easy_setopt(ctx.curl, CURLOPT_WRITEFUNCTION, receive_callback);
        curl_easy_setopt(ctx.curl, CURLOPT_WRITEDATA, &resp_body);

        CURLcode curl_code = curl_easy_perform(ctx.curl);
        if (curl_code != CURLE_OK) {
            throw std::exception();
        }

        long status_code = 0;
        curl_easy_getinfo(ctx.curl, CURLINFO_RESPONSE_CODE, &status_code);

        response r;
        r.status_code = status_code;
        r.body = resp_body;

        return r;
    }

    static http_client::response get(std::string url, std::vector<std::string> headers) {
        struct context {
            CURL* curl;
            curl_slist* chunk;
            context() : curl(nullptr), chunk(nullptr) {}
            ~context() {
                if (curl != nullptr) curl_easy_cleanup(curl);
                if (chunk != nullptr) curl_slist_free_all(chunk);
            }
        } ctx;

        ctx.curl = curl_easy_init();

        // set url
        curl_easy_setopt(ctx.curl, CURLOPT_URL, url.c_str());

        // set header
        for (auto&& header: headers) {
            ctx.chunk = curl_slist_append(ctx.chunk, header.c_str());
        }
        curl_easy_setopt(ctx.curl, CURLOPT_HTTPHEADER, ctx.chunk);

        // set response callback
        std::string resp_body;
        curl_easy_setopt(ctx.curl, CURLOPT_WRITEFUNCTION, receive_callback);
        curl_easy_setopt(ctx.curl, CURLOPT_WRITEDATA, &resp_body);

        CURLcode curl_code = curl_easy_perform(ctx.curl);
        if (curl_code != CURLE_OK) {
            throw std::exception();
        }

        long status_code = 0;
        curl_easy_getinfo(ctx.curl, CURLINFO_RESPONSE_CODE, &status_code);

        response r;
        r.status_code = status_code;
        r.body = resp_body;

        return r;
    }
};

#endif // HTTP_CLIENT_H_INCLUDED
