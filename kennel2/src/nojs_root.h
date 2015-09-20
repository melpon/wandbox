#ifndef NOJS_ROOT_H_INCLUDED
#define NOJS_ROOT_H_INCLUDED

#include <string>
#include <set>
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

        // twitter cards
        void set_twitter(std::string title, std::string description) {
            std::string tmpl = " - Wandbox";
            twitter_title = title.substr(0, 70 - tmpl.size()) + tmpl;
            twitter_description = description.substr(0, 200);
        }
        std::string twitter_title;
        std::string twitter_description;

        // permlink
        template<class F>
        void split(const std::string& str, char delim, F inserter) {
            std::size_t current = 0, found;
            while ((found = str.find_first_of(delim, current)) != std::string::npos){
                inserter(str.substr(current, found - current));
                current = found + 1;
            }
            inserter(str.substr(current, str.size() - current));
        }
        cppcms::json::value permlink;
        std::set<std::string> options;
        void set_permlink(cppcms::json::value permlink) {
            this->permlink = std::move(permlink);

            auto str = permlink["options"].str();
            split(str, ',', [&options](const std::string& str) { options.insert(str); });
        }
        std::string code() const {
            return permlink.is_undefined() ? "" : permlink["code"].str();
        }

        bool has_checkbox_option(cppcms::json::value& sw) const {
            return permlink.is_undefined() ? sw["default"].boolean() : options.find(sw["name"].str()) != options.end();
        }
        bool has_select_option(cppcms::json::value& sw, cppcms::json::value& opt) const {
            return permlink.is_undefined() ? opt["name"].str() == sw["default"].str() : options.find(opt["name"].str()) != options.end();
        }
        std::string compiler_option_raw() const {
            return permlink.is_undefined() ? "" : permlink.get("compiler-option-raw", "");
        }
        std::string runtime_option_raw() const {
            return permlink.is_undefined() ? "" : permlink.get("runtime-option-raw", "");
        }
        std::string stdin() const {
            return permlink.is_undefined() ? "" : permlink.get("stdin", "");
        }
        std::string compiler_message() const {
            return permlink.is_undefined() ? "" : permlink.get("compiler_message", "");
        }
        std::string program_message() const {
            return permlink.is_undefined() ? "" : permlink.get("program_message", "");
        }
    };
}

#endif // NOJS_ROOT_H_INCLUDED
