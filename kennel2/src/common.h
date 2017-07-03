#ifndef COMMON_H_INCLUDED
#define COMMON_H_INCLUDED

#include "libs.h"
#include <string>

namespace content {
    struct common : public cppcms::base_content {
        common(cppcms::service& srv) {
            login = false;
            login_url = "https://github.com/login/oauth/authorize?client_id=" + srv.settings()["application"]["github"]["client_id"].str();
            google_analytics = srv.settings()["application"]["google_analytics"].str();
        }

        bool login;
        std::string login_url;
        struct login_info_t {
            std::string name;
            std::string avatar_url;
        };
        login_info_t login_info;
        void set_login(login_info_t info) {
            login = true;
            login_info = info;
        }

        std::string google_analytics;
    };
}

#endif // COMMON_H_INCLUDED
