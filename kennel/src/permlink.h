#ifndef PERMLINK_H_
#define PERMLINK_H_

#include <cppdb/frontend.h>

#include <algorithm>
#include <ctime>
#include <iostream>
#include <string>
#include <vector>

#include "kennel.json.h"

class permlink {
  cppdb::session sql;

 public:
  permlink(const std::string& database) : sql(database) {}
  void init() {
    cppdb::transaction guard(sql);

    sql << "CREATE TABLE IF NOT EXISTS code ("
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
           "  CONSTRAINT unique_code UNIQUE (compiler, code, optimize, "
           "    warning, created_at)"
           ")"
        << cppdb::exec;

    sql << "CREATE TABLE IF NOT EXISTS codes ("
           "  id                   INTEGER PRIMARY KEY,"
           "  code_id              INTEGER NOT NULL REFERENCES code,"
           "  \"order\"            INTEGER NOT NULL,"
           "  file                 VARCHAR NOT NULL,"
           "  code                 VARCHAR NOT NULL,"
           "  CONSTRAINT unique_codes UNIQUE (code_id, \"order\")"
           ")"
        << cppdb::exec;

    sql << "CREATE TABLE IF NOT EXISTS compiler_info ("
           "  id                   INTEGER PRIMARY KEY,"
           "  code_id              INTEGER NOT NULL REFERENCES code,"
           "  json                 VARCHAR NOT NULL,"
           "  CONSTRAINT unique_compiler_info UNIQUE (code_id)"
           ")"
        << cppdb::exec;

    sql << "CREATE TABLE IF NOT EXISTS link ("
           "  id           INTEGER PRIMARY KEY,"
           "  permlink     VARCHAR NOT NULL,"
           "  code_id      INTEGER NOT NULL REFERENCES code,"
           "  CONSTRAINT unique_link UNIQUE (permlink)"
           ")"
        << cppdb::exec;

    sql << "CREATE TABLE IF NOT EXISTS link_output ("
           "  id           INTEGER PRIMARY KEY,"
           "  link_id      INTEGER NOT NULL REFERENCES link,"
           "  \"order\"    INTEGER NOT NULL,"
           "  type         VARCHAR NOT NULL,"
           "  output       VARCHAR NOT NULL,"
           "  CONSTRAINT unique_link_output UNIQUE (link_id, \"order\")"
           ")"
        << cppdb::exec;

    sql << "CREATE TABLE IF NOT EXISTS github_user ("
           "  id           INTEGER PRIMARY KEY,"
           "  username     VARCHAR NOT NULL,"
           "  created_at   TIMESTAMP NOT NULL,"
           "  updated_at   TIMESTAMP NOT NULL,"
           "  CONSTRAINT unique_name UNIQUE (username)"
           ")"
        << cppdb::exec;

    migrate();

    sql.commit();
  }

 private:
  void add_column(std::string table_name, std::string column_name,
                  std::string column_type) {
    cppdb::result r;

    r = sql << "PRAGMA table_info(" + table_name + ")";

    /*
        0|id|INTEGER|0||1
        1|compiler|VARCHAR|1||0
        ...
        8|stdin|VARCHAR|1|""|0
        9|created_at|TIMESTAMP|1||0
        */
    std::vector<std::string> columns;
    while (r.next()) {
      columns.push_back(r.get<std::string>(1));
    }
    auto it = std::find(columns.begin(), columns.end(), column_name);
    if (it == columns.end()) {
      sql << ("ALTER TABLE " + table_name + " ADD COLUMN " + column_name + " " +
              column_type)
          << cppdb::exec;
    }
  }

  void migrate() {
    add_column("code", "title", "VARCHAR NOT NULL DEFAULT \"\"");
    add_column("code", "description", "VARCHAR NOT NULL DEFAULT \"\"");
    add_column("code", "github_user", "VARCHAR NOT NULL DEFAULT \"\"");
    add_column("code", "private", "BOOLEAN NOT NULL DEFAULT 0");
    add_column("code", "updated_at", "TIMESTAMP");
    sql << "CREATE INDEX IF NOT EXISTS link_code ON link (code_id)"
        << cppdb::exec;
    sql << "CREATE INDEX IF NOT EXISTS github_user_list "
           "ON code (github_user, private, created_at DESC)"
        << cppdb::exec;
    add_column("github_user", "github_access_token",
               "VARCHAR NOT NULL DEFAULT \"\"");
    add_column("github_user", "wandbox_access_token",
               "VARCHAR NOT NULL DEFAULT \"\"");
    sql << "CREATE INDEX IF NOT EXISTS github_user_wandbox_access_token "
           "ON github_user (wandbox_access_token)"
        << cppdb::exec;
  }

 public:
  void make_permlink(std::string permlink_name,
                     wandbox::kennel::PostPermlinkRequest req,
                     const wandbox::kennel::CompilerInfo& compiler_info) {
    std::time_t now_time = std::time(nullptr);
    std::tm now = *std::localtime(&now_time);

    cppdb::transaction guard(sql);

    cppdb::statement stat;

    stat =
        sql << "INSERT INTO code ( "
               "  title, description, compiler, code, optimize, "
               "  warning, options, compiler_option_raw, "
               "  runtime_option_raw, stdin, created_at, updated_at, "
               "  github_user "
               ") "
               "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            << req.title << req.description << req.compiler << req.code << false
            << false << req.options << req.compiler_option_raw
            << req.runtime_option_raw << req.stdin << now << now
            << req.github_user << cppdb::exec;

    auto code_id = stat.last_insert_id();
    //std::clog << code_id << std::endl;

    if (!req.codes.empty()) {
      int order = 1;
      stat = sql << "INSERT INTO codes (code_id, \"order\", file, code) "
                    "VALUES (?, ?, ?, ?)";
      for (const auto& v : req.codes) {
        stat << code_id << order << v.file << v.code << cppdb::exec;
        stat.reset();
        order += 1;
      }
    }

    std::string cistr = jsonif::to_json(compiler_info);
    stat = sql << "INSERT INTO compiler_info (code_id, json) "
                  "VALUES (?, ?)"
               << code_id << cistr << cppdb::exec;
    //std::clog << "compiler_info id: " << stat.last_insert_id() << std::endl;

    stat = sql << "INSERT INTO link (permlink, code_id) "
                  "VALUES (?, ?)"
               << permlink_name << code_id << cppdb::exec;

    auto link_id = stat.last_insert_id();
    //std::clog << "link_id: " << link_id << std::endl;

    int order = 1;
    stat = sql << "INSERT INTO link_output (link_id, \"order\", type, output) "
                  "VALUES (?, ?, ?, ?)";
    for (const auto& output : req.results) {
      stat << link_id << order << output.type << output.data << cppdb::exec;
      stat.reset();
      order += 1;
    }

    sql.commit();
  }

  wandbox::kennel::GetPermlinkResponse get_permlink(std::string permlink_name) {
    wandbox::kennel::GetPermlinkResponse p;

    cppdb::result r;
    r = sql << "SELECT id, code_id FROM link WHERE permlink=? " << permlink_name
            << cppdb::row;

    auto link_id = r.get<int>("id");
    auto code_id = r.get<int>("code_id");

    r = sql << "SELECT "
               "  compiler, code, options, compiler_option_raw, "
               "  runtime_option_raw, stdin, created_at, title, description, "
               "  github_user, private "
               "FROM code "
               "WHERE id=?"
            << code_id << cppdb::row;
    p.parameter.compiler = r.get<std::string>("compiler");
    p.parameter.code = r.get<std::string>("code");
    p.parameter.options = r.get<std::string>("options");
    p.parameter.compiler_option_raw = r.get<std::string>("compiler_option_raw");
    p.parameter.runtime_option_raw = r.get<std::string>("runtime_option_raw");
    p.parameter.stdin = r.get<std::string>("stdin");
    auto tm = r.get<std::tm>("created_at");
    auto created_at = std::mktime(&tm);
    // auto local_created_at = *std::localtime(&time);
    // char buf[128];
    // auto size = std::strftime(buf, sizeof(buf), "%FT%T", &local_created_at);
    p.parameter.created_at = created_at;
    p.parameter.title = r.get<std::string>("title");
    p.parameter.description = r.get<std::string>("description");
    p.parameter.github_user = r.get<std::string>("github_user");
    p.parameter.is_private = r.get<int>("private") != 0;

    r = sql << "SELECT file, code "
               "FROM codes "
               "WHERE code_id=? "
               "ORDER BY \"order\""
            << code_id;
    {
      while (r.next()) {
        wandbox::kennel::Code code;
        code.file = r.get<std::string>("file");
        code.code = r.get<std::string>("code");
        p.parameter.codes.push_back(std::move(code));
      }
    }

    r = sql << "SELECT type, output "
               "FROM link_output "
               "WHERE link_id=? "
               "ORDER BY \"order\""
            << link_id;
    {
      while (r.next()) {
        wandbox::kennel::CompileNdjsonResult result;
        result.type = r.get<std::string>("type");
        result.data = r.get<std::string>("output");
        p.results.push_back(std::move(result));
      }
    }

    // load compiler_info if exists.
    r = sql << "SELECT json "
               "FROM compiler_info "
               "WHERE code_id=?"
            << code_id << cppdb::row;
    if (!r.empty()) {
      p.parameter.compiler_info = jsonif::from_json<wandbox::kennel::CompilerInfo>(
          r.get<std::string>("json"));
    }
    return p;
  }

  struct usercode_info {
    int current_page;
    int page_max;
    int rows_per_page;
    struct code_t {
      std::string compiler;
      std::string code;
      std::string options;
      std::time_t created_at;
      std::string title;
      std::string description;
      std::string github_user;
      bool is_private;
      std::string permlink;
    };
    std::vector<code_t> codes;
  };
  usercode_info get_github_usercode(std::string username, bool include_private,
                                    int current_page, int rows_per_page) {
    cppdb::result r;
    r = sql << "SELECT COUNT(*) as count "
               "FROM code "
               "WHERE github_user=? AND private<=? "
               "ORDER BY created_at DESC "
            << username << (include_private ? 1 : 0) << cppdb::row;
    auto page_max = (r.get<int>("count") + rows_per_page - 1) / rows_per_page;

    r = sql << "SELECT "
               "  compiler, code, options, created_at, title, "
               "  description, github_user, private, "
               "  link.permlink as permlink "
               "FROM code, link "
               "WHERE github_user=? AND private<=? AND code.id=link.code_id "
               "ORDER BY created_at DESC "
               "LIMIT ? OFFSET ?"
            << username << (include_private ? 1 : 0) << rows_per_page
            << (current_page * rows_per_page);

    usercode_info info;
    info.current_page = current_page;
    info.page_max = page_max;
    info.rows_per_page = rows_per_page;
    while (r.next()) {
      usercode_info::code_t code;
      code.compiler = r.get<std::string>("compiler");
      code.code = r.get<std::string>("code");
      code.options = r.get<std::string>("options");
      std::tm tm = r.get<std::tm>("created_at");
      code.created_at = std::mktime(&tm);
      code.title = r.get<std::string>("title");
      code.description = r.get<std::string>("description");
      code.github_user = r.get<std::string>("github_user");
      code.is_private = r.get<int>("private") != 0;
      code.permlink = r.get<std::string>("permlink");
      info.codes.push_back(std::move(code));
    }

    return info;
  }
};

#endif  // PERMLINK_H_
