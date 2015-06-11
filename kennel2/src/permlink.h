#ifndef PERMLINK_H_INCLUDED
#define PERMLINK_H_INCLUDED

#include <iostream>
#include <ctime>
#include "libs.h"

class permlink {
    cppdb::session sql;
public:
    permlink(cppcms::service& service) : sql(service.settings()["application"]["database"].str()) {
    }
    void init() {
        cppdb::transaction guard(sql);

        sql <<
            "CREATE TABLE IF NOT EXISTS code ("
            "  id                   INTEGER PRIMARY KEY,"
            "  compiler             VARCHAR NOT NULL,"
            "  code                 VARCHAR NOT NULL,"
            "  optimize             BOOLEAN NOT NULL,"
            "  warning              BOOLEAN NOT NULL,"
            "  options              VARCHAR NOT NULL,"
            "  compiler_option_raw  VARCHAR NOT NULL DEFAULT \"\","
            "  runtime_option_raw   VARCHAR NOT NULL DEFAULT \"\","
            "  stdin                VARCHAR NOT NULL DEFAULT \"\","
            "  created_at           TIMESTAMP NOT NULL,"
            "  CONSTRAINT unique_code UNIQUE (compiler, code, optimize, warning, created_at)"
            ")"
            << cppdb::exec;

        sql <<
            "CREATE TABLE IF NOT EXISTS codes ("
            "  id                   INTEGER PRIMARY KEY,"
            "  code_id              INTEGER NOT NULL REFERENCES code,"
            "  \"order\"            INTEGER NOT NULL,"
            "  file                 VARCHAR NOT NULL,"
            "  code                 VARCHAR NOT NULL,"
            "  CONSTRAINT unique_codes UNIQUE (code_id, \"order\")"
            ")"
            << cppdb::exec;

        sql <<
            "CREATE TABLE IF NOT EXISTS compiler_info ("
            "  id                   INTEGER PRIMARY KEY,"
            "  code_id              INTEGER NOT NULL REFERENCES code,"
            "  json                 VARCHAR NOT NULL,"
            "  CONSTRAINT unique_compiler_info UNIQUE (code_id)"
            ")"
            << cppdb::exec;

        sql <<
            "CREATE TABLE IF NOT EXISTS link ("
            "  id           INTEGER PRIMARY KEY,"
            "  permlink     VARCHAR NOT NULL,"
            "  code_id      INTEGER NOT NULL REFERENCES code,"
            "  CONSTRAINT unique_link UNIQUE (permlink)"
            ")"
            << cppdb::exec;

        sql <<
            "CREATE TABLE IF NOT EXISTS link_output ("
            "  id           INTEGER PRIMARY KEY,"
            "  link_id      INTEGER NOT NULL REFERENCES link,"
            "  \"order\"    INTEGER NOT NULL,"
            "  type         VARCHAR NOT NULL,"
            "  output       VARCHAR NOT NULL,"
            "  CONSTRAINT unique_link_output UNIQUE (link_id, \"order\")"
            ")"
            << cppdb::exec;

        sql.commit();
    }

    void make_permlink(std::string permlink_name, cppcms::json::value code, cppcms::json::value compiler_info) {
        std::time_t now_time = std::time(nullptr);
        std::tm now = *std::gmtime(&now_time);

        cppdb::transaction guard(sql);

        cppdb::statement stat;

        stat = sql <<
            "INSERT INTO code (compiler, code, optimize, warning, options, compiler_option_raw, runtime_option_raw, stdin, created_at) "
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
            << code["compiler"].str()
            << code["code"].str()
            << false
            << false
            << code.get("options", "")
            << code.get("compiler-option-raw", "")
            << code.get("runtime-option-raw", "")
            << code.get("stdin", "")
            << now
            << cppdb::exec;

        auto code_id = stat.last_insert_id();
        //std::clog << code_id << std::endl;

        if (!code["codes"].is_undefined()) {
            int order = 1;
            stat = sql <<
                "INSERT INTO codes (code_id, \"order\", file, code) "
                "VALUES (?, ?, ?, ?)";
            for (auto&& v: code["codes"].array()) {
                stat
                    << code_id
                    << order
                    << v["file"].str()
                    << v["code"].str()
                    << cppdb::exec;
                stat.reset();
                order += 1;
            }
        }

        std::stringstream ss;
        compiler_info.save(ss, cppcms::json::compact);
        stat = sql <<
            "INSERT INTO compiler_info (code_id, json) "
            "VALUES (?, ?)"
            << code_id
            << ss.str()
            << cppdb::exec;
        //std::clog << "compiler_info id: " << stat.last_insert_id() << std::endl;

        stat = sql <<
            "INSERT INTO link (permlink, code_id) "
            "VALUES (?, ?)"
            << permlink_name
            << code_id
            << cppdb::exec;

        auto link_id = stat.last_insert_id();
        //std::clog << link_id << std::endl;

        int order = 1;
        stat = sql <<
            "INSERT INTO link_output (link_id, \"order\", type, output) "
            "VALUES (?, ?, ?, ?)";
        for (auto&& output: code["outputs"].array()) {
            stat
                << link_id
                << order
                << output["type"].str()
                << output["output"].str()
                << cppdb::exec;
            stat.reset();
            order += 1;
        }

        sql.commit();
    }
    cppcms::json::value get_permlink(std::string permlink_name) {
        cppdb::result r;
        r = sql <<
            "SELECT id, code_id FROM link WHERE permlink=? "
            << permlink_name
            << cppdb::row;

        auto link_id = r.get<int>("id");
        auto code_id = r.get<int>("code_id");

        r = sql <<
            "SELECT compiler, code, options, compiler_option_raw, runtime_option_raw, stdin, created_at "
            "FROM code "
            "WHERE id=?"
            << code_id
            << cppdb::row;
        cppcms::json::value value;
        value["compiler"] = r.get<std::string>("compiler");
        value["code"] = r.get<std::string>("code");
        value["options"] = r.get<std::string>("options");
        value["compiler-option-raw"] = r.get<std::string>("compiler_option_raw");
        value["runtime-option-raw"] = r.get<std::string>("runtime_option_raw");
        value["stdin"] = r.get<std::string>("stdin");
        auto created_at = r.get<std::tm>("created_at");
        auto time = std::mktime(&created_at);
        auto local_created_at = *std::localtime(&time);
        char buf[128];
        auto size = std::strftime(buf, sizeof(buf), "%FT%T", &local_created_at);
        value["created-at"] = std::string(buf, size);

        r = sql <<
            "SELECT file, code "
            "FROM codes "
            "WHERE code_id=? "
            "ORDER BY \"order\""
            << code_id;
        {
            int n = 0;
            while (r.next()) {
                value["codes"][n]["file"] = r.get<std::string>("file");
                value["codes"][n]["code"] = r.get<std::string>("code");
                n += 1;
            }
        }

        r = sql <<
            "SELECT type, output "
            "FROM link_output "
            "WHERE link_id=? "
            "ORDER BY \"order\""
            << link_id;
        {
            int n = 0;
            while (r.next()) {
                value["outputs"][n]["type"] = r.get<std::string>("type");
                value["outputs"][n]["output"] = r.get<std::string>("output");
                n += 1;
            }
        }

        // load compiler_info if exists.
        r = sql <<
            "SELECT json "
            "FROM compiler_info "
            "WHERE code_id=?"
            << code_id
            << cppdb::row;
        if (!r.empty()) {
            cppcms::json::value compiler_info;
            std::stringstream ss(r.get<std::string>("json"));
            if (compiler_info.load(ss, true)) {
                value["compiler-info"] = compiler_info;
            }
        }
        return value;
    }
};

#endif // PERMLINK_H_INCLUDED
