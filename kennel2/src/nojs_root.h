#ifndef NOJS_ROOT_H_INCLUDED
#define NOJS_ROOT_H_INCLUDED

#include <string>
#include "libs.h"

namespace content {
    struct nojs_root : public cppcms::base_content {
        cppcms::json::value compiler_info;
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
    };
}

#endif // NOJS_ROOT_H_INCLUDED
