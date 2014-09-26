#include <cppcms/view.h>
#include <cppcms/json.h>
#include <string>

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
    };
}
