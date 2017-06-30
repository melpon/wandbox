#ifndef USER_H_INCLUDED
#define USER_H_INCLUDED

#include "libs.h"
#include "root.h"
#include "permlink.h"

namespace content {
    struct user : public cppcms::base_content {
        user(cppcms::service& srv) {
            login = false;
            login_url = "https://github.com/login/oauth/authorize?client_id=" + srv.settings()["application"]["github"]["client_id"].str();
            google_analytics = srv.settings()["application"]["google_analytics"].str();
        }

        bool login;
        std::string login_url;
        typedef content::root::login_info_t login_info_t;
        login_info_t login_info;
        void set_login(content::root::login_info_t info) {
            login = true;
            login_info = info;
        }

        std::string google_analytics;

        struct target_user_info_t {
            std::string username;
            std::string avatar_url;
            permlink::usercode_info usercode;
        };
        target_user_info_t target;
        void set_target(target_user_info_t target) {
            this->target = target;
        }

        static const int head_lines = 9;
        std::string head(std::string code) {
            std::vector<std::string> lines;
            std::string::size_type pos = 0;
            for (auto i = 0; i < head_lines; i++) {
                auto pos2 = code.find('\n', pos);
                if (pos2 == std::string::npos) return code;
                lines.push_back(code.substr(pos, pos2 - pos));
                pos = pos2 + 1;
            }
            // remove empty line
            while (true) {
                if (lines.empty()) break;
                if (lines.back().size() != 0) break;
                lines.pop_back();
            }
            // join
            std::string str;
            for (const auto& line: lines) {
                str += line;
                str += '\n';
            }
            return str;
        }
    };
}

#endif // USER_H_INCLUDED
