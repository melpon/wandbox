#ifndef ROOT_H_INCLUDED
#define ROOT_H_INCLUDED

#include <string>
#include <fstream>
#include <cstring>
#include <ctime>
#include "libs.h"

namespace content {
    struct root : public cppcms::base_content {
        root(cppcms::service& srv) {
            using_permlink = false;
            permlink = "null";
            init_sponsors(srv);

            google_analytics = srv.settings()["application"]["google_analytics"].str();
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

        struct sponsor {
            std::string name;
            std::string url;
            std::time_t due_date;
        };
        bool has_sponsors;
        std::vector<sponsor> corporate_sponsors;
        std::vector<sponsor> personal_sponsors;
        std::time_t from_iso8601(std::string str) {
            std::tm tm;
            std::memset(&tm, 0, sizeof(tm));
            strptime(str.c_str(), "%FT%T%z", &tm);
            return std::mktime(&tm);
        }
        sponsor make_sponsor(cppcms::json::value& json) {
            sponsor sp;
            sp.name = json["name"].str();
            sp.url = json["url"].str();
            sp.due_date = from_iso8601(json["due_date"].str());
            return sp;
        }

        void init_sponsors(cppcms::service& srv) {
            has_sponsors = false;

            auto file = srv.settings()["application"]["sponsors"].str();
            if (file.empty()) {
                return;
            }

            std::ifstream ifs(file.c_str());
            if (!ifs) {
                return;
            }

            cppcms::json::value js;
            if (!js.load(ifs, true, nullptr)) {
                return;
            }

            auto now = std::time(nullptr);
            for (auto&& v: js["corporate"].array()) {
                auto sp = make_sponsor(v);
                if (now <= sp.due_date) {
                    corporate_sponsors.push_back(std::move(sp));
                }
            }
            for (auto&& v: js["personal"].array()) {
                auto sp = make_sponsor(v);
                if (now <= sp.due_date) {
                    personal_sponsors.push_back(std::move(sp));
                }
            }
            has_sponsors = true;
        }

        std::string google_analytics;
    };
}

#endif // ROOT_H_INCLUDED
