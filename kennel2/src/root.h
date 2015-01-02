#ifndef ROOT_H_INCLUDED
#define ROOT_H_INCLUDED

#include <string>
#include "libs.h"

namespace content {
    struct root : public cppcms::base_content {
        root() {
            using_permlink = false;
            permlink = "null";
        }

        cppcms::json::value compiler_infos;
        cppcms::json::value sw; // loop variable
        cppcms::json::value opt; // loop variable
        cppcms::json::value get(cppcms::json::value& v, const std::string& key) {
            return v[key];
        }
        std::string getstr(cppcms::json::value& v, const std::string& key) {
            return v[key].str();
        }
        bool getbool(cppcms::json::value& v, const std::string& key) {
            return v[key].boolean();
        }

        bool using_permlink;
        std::string permlink;

        void set_twitter(std::string title, std::string description) {
            std::string tmpl = " - Wandbox";
            twitter_title = title.substr(0, 70 - tmpl.size()) + tmpl;
            twitter_description = description.substr(0, 200);
        }
        std::string twitter_title;
        std::string twitter_description;
    };
}

#endif // ROOT_H_INCLUDED
