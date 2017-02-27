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

        void set_compiler_infos(cppcms::json::value compiler_infos) {
            this->compiler_infos = compiler_infos;
            {
                int n = 0;
                for (auto&& info: compiler_infos.array()) {
                    auto lang = info["language"].str();
                    langinfos[lang].language = lang;
                    langinfos[lang].compilers.push_back(info);
                    langinfos[lang].tab_id = "language-tab-" + std::to_string(n);
                    langinfos[lang].count += 1;
                    n++;
                }
            }
            {
                const int width = 2;
                std::vector<language_info*> tmp_langinfos(langinfos.size());
                std::transform(langinfos.begin(), langinfos.end(), tmp_langinfos.begin(), [](std::map<std::string, language_info>::value_type& v) {
                    return &v.second;
                });
                vertical_langinfos.resize(langinfos.size());
                int x = 0;
                for (int i = 0; i < width; i++) {
                    for (int n = i; n < (int)langinfos.size(); n += width) {
                        vertical_langinfos[n] = tmp_langinfos[x++];
                    }
                }
            }
            {
                int n = 0;
                for (auto&& info: compiler_infos.array()) {
                    auto name = info["name"].str();
                    compinfos[name].info = info;
                    compinfos[name].tab_id = "compiler-tab-" + std::to_string(n);
                    n++;
                }
            }
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

        struct language_info {
            std::string language;
            std::vector<cppcms::json::value> compilers;
            std::string tab_id;
            int count;
            language_info() : count(0) { }
        };
        std::map<std::string, language_info> langinfos;
        std::vector<language_info*> vertical_langinfos;
        struct compiler_info {
            cppcms::json::value info;
            std::string tab_id;
        };
        std::map<std::string, compiler_info> compinfos;

        std::string lookup_compiler_tab_id(cppcms::json::value& info) {
            return compinfos[info["name"].str()].tab_id;
        }

        std::string make_full_name(cppcms::json::value& info) {
            return "[" + info["language"].str() + "] " + info["display-name"].str() + " " + info["version"].str();
        }
    };
}

#endif // ROOT_H_INCLUDED
