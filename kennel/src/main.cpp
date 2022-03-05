#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <memory>
#include <queue>
#include <string>

// spdlog
#include <spdlog/spdlog.h>

// Boost
#include <boost/asio.hpp>
#include <boost/beast.hpp>
#include <boost/json.hpp>

// CLI11
#include <CLI/CLI.hpp>

#include "cattleshed_client.h"
#include "kennel.json.h"
#include "permlink.h"

// wandbox::cattleshed::GetVersionResponse を頑張って wandbox::kennel::CattleshedInfo に変換する
static wandbox::kennel::CattleshedInfo version_response_to_kennel(
    const wandbox::cattleshed::GetVersionResponse& resp) {
  wandbox::kennel::CattleshedInfo r;
  for (int i = 0; i < resp.compiler_info_size(); i++) {
    wandbox::kennel::CompilerInfo info;

    const wandbox::cattleshed::CompilerInfo& c = resp.compiler_info(i);
    info.name = c.name();
    info.version = c.version();
    info.language = c.language();
    info.display_name = c.display_name();
    for (int j = 0; j < c.templates_size(); j++) {
      info.templates.push_back(c.templates(j));
    }
    info.compiler_option_raw = c.compiler_option_raw();
    info.runtime_option_raw = c.runtime_option_raw();
    info.display_compile_command = c.display_compile_command();
    for (int j = 0; j < c.switches_size(); j++) {
      wandbox::kennel::Switch sw;

      const wandbox::cattleshed::Switch& s = c.switches(j);
      if (s.data_case() == wandbox::cattleshed::Switch::kSingle) {
        const wandbox::cattleshed::SingleSwitch& ss = s.single();
        sw.type = "single";
        sw.name = ss.name();
        sw.default_value = ss.default_value();
        sw.display_name = ss.display_name();
        sw.display_flags = ss.display_flags();
      } else {
        const wandbox::cattleshed::SelectSwitch& ss = s.select();
        sw.type = "select";
        sw.name = ss.name();
        sw.default_value = ss.default_value();
        for (int k = 0; k < ss.options_size(); k++) {
          wandbox::kennel::SelectSwitchOption option;
          const wandbox::cattleshed::SelectSwitchOption& opt = ss.options(k);
          option.name = opt.name();
          option.display_name = opt.display_name();
          option.display_flags = opt.display_flags();
          sw.options.push_back(std::move(option));
        }
      }

      info.switches.push_back(std::move(sw));
    }

    r.compilers.push_back(std::move(info));
  }

  for (int i = 0; i < resp.templates_size(); i++) {
    wandbox::kennel::Template tmpl;
    {
      const wandbox::cattleshed::Template& t = resp.templates(i);
      tmpl.name = t.name();
      tmpl.code = t.default_source();
      for (int j = 0; j < t.sources_size(); j++) {
        wandbox::kennel::Code code;
        code.file = t.sources(j).file_name();
        code.code = t.sources(j).source();
        tmpl.codes.push_back(std::move(code));
      }
      tmpl.stdin = t.stdin();
      tmpl.options = t.compiler_options();
      tmpl.compiler_option_raw = t.compiler_option_raw();
      tmpl.runtime_option_raw = t.runtime_option_raw();
    }
    r.templates.push_back(std::move(tmpl));
  }

  return r;
}

struct KennelSharedDataConfig {
  boost::asio::io_context* ioc;
  std::shared_ptr<CattleshedClientManager> cm;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> initial_cattleshed_info;
};

// すべての接続で共有するデータ
class KennelSharedData : public std::enable_shared_from_this<KennelSharedData> {
 public:
  KennelSharedData(KennelSharedDataConfig config) : config_(std::move(config)) {
    cattleshed_info_ = config_.initial_cattleshed_info;
  }

  void MaybeUpdateCattleshedInfo() {
    auto now = std::chrono::steady_clock::now();
    auto elapsed = now - last_updated_;
    if (elapsed <= std::chrono::hours(1)) {
      return;
    }
    last_updated_ = now;

    auto client = config_.cm->CreateGetVersionClient();
    client->SetOnFinish([self = shared_from_this(), client](
                            wandbox::cattleshed::GetVersionResponse resp,
                            grpc::Status status) {
      if (!status.ok()) {
        return;
      }
      boost::asio::post(*self->config_.ioc, [self, resp = std::move(resp)]() {
        self->cattleshed_info_.reset(new wandbox::kennel::CattleshedInfo(
            version_response_to_kennel(resp)));
      });
    });
    wandbox::cattleshed::GetVersionRequest req;
    client->Connect(req);
  }

  // 必要になったらロックを掛けてコピーする実装に変える
  std::shared_ptr<wandbox::kennel::CattleshedInfo> GetCattleshedInfo() const {
    return cattleshed_info_;
  }

 private:
  std::chrono::steady_clock::time_point last_updated_;
  KennelSharedDataConfig config_;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> cattleshed_info_;
};

boost::beast::http::response<boost::beast::http::string_body> BadRequest(
    const boost::beast::http::request<boost::beast::http::string_body>& req,
    boost::beast::string_view why) {
  boost::beast::http::response<boost::beast::http::string_body> res{
      boost::beast::http::status::bad_request, req.version()};
  res.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
  res.set(boost::beast::http::field::content_type, "text/html");
  res.keep_alive(req.keep_alive());
  res.body() = why.to_string();
  res.prepare_payload();
  return res;
}

boost::beast::http::response<boost::beast::http::string_body> NotFound(
    const boost::beast::http::request<boost::beast::http::string_body>& req,
    boost::beast::string_view target) {
  boost::beast::http::response<boost::beast::http::string_body> res{
      boost::beast::http::status::not_found, req.version()};
  res.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
  res.set(boost::beast::http::field::content_type, "text/html");
  res.keep_alive(req.keep_alive());
  res.body() = "The resource '" + target.to_string() + "' was not found.";
  res.prepare_payload();
  return res;
}

static void update_compile_result(
    wandbox::kennel::CompileResult& result,
    const wandbox::cattleshed::RunJobResponse& resp) {
  if (resp.type() == wandbox::cattleshed::RunJobResponse::CONTROL) {
  } else if (resp.type() ==
             wandbox::cattleshed::RunJobResponse::COMPILER_STDOUT) {
    result.compiler_output += resp.data();
    result.compiler_message += resp.data();
  } else if (resp.type() ==
             wandbox::cattleshed::RunJobResponse::COMPILER_STDERR) {
    result.compiler_error += resp.data();
    result.compiler_message += resp.data();
  } else if (resp.type() == wandbox::cattleshed::RunJobResponse::STDOUT) {
    result.program_output += resp.data();
    result.program_message += resp.data();
  } else if (resp.type() == wandbox::cattleshed::RunJobResponse::STDERR) {
    result.program_error += resp.data();
    result.program_message += resp.data();
  } else if (resp.type() == wandbox::cattleshed::RunJobResponse::EXIT_CODE) {
    result.status += resp.data();
  } else if (resp.type() == wandbox::cattleshed::RunJobResponse::SIGNAL) {
    result.signal += resp.data();
  } else {
    //append(result["error"], resp.data());
  }
}

static std::string response_type_to_string(
    wandbox::cattleshed::RunJobResponse::Type type) {
  switch (type) {
    case wandbox::cattleshed::RunJobResponse::CONTROL:
      return "Control";
    case wandbox::cattleshed::RunJobResponse::COMPILER_STDOUT:
      return "CompilerMessageS";
    case wandbox::cattleshed::RunJobResponse::COMPILER_STDERR:
      return "CompilerMessageE";
    case wandbox::cattleshed::RunJobResponse::STDOUT:
      return "StdOut";
    case wandbox::cattleshed::RunJobResponse::STDERR:
      return "StdErr";
    case wandbox::cattleshed::RunJobResponse::EXIT_CODE:
      return "ExitCode";
    case wandbox::cattleshed::RunJobResponse::SIGNAL:
      return "Signal";
    default:
      return "";
  }
}

static wandbox::cattleshed::RunJobResponse::Type string_to_response_type(
    std::string str) {
  if (str == "Control") {
    return wandbox::cattleshed::RunJobResponse::CONTROL;
  } else if (str == "CompilerMessageS") {
    return wandbox::cattleshed::RunJobResponse::COMPILER_STDOUT;
  } else if (str == "CompilerMessageE") {
    return wandbox::cattleshed::RunJobResponse::COMPILER_STDERR;
  } else if (str == "StdOut") {
    return wandbox::cattleshed::RunJobResponse::STDOUT;
  } else if (str == "StdErr") {
    return wandbox::cattleshed::RunJobResponse::STDERR;
  } else if (str == "ExitCode") {
    return wandbox::cattleshed::RunJobResponse::EXIT_CODE;
  } else if (str == "Signal") {
    return wandbox::cattleshed::RunJobResponse::SIGNAL;
  }
  return wandbox::cattleshed::RunJobResponse::CONTROL;
}

static wandbox::cattleshed::Issuer make_issuer(
    const boost::beast::http::request<boost::beast::http::string_body>& req,
    std::string github_user) {
  wandbox::cattleshed::Issuer issuer;
  issuer.set_remote_addr("");
  auto it = req.find("X-Real-IP");
  if (it != req.end()) {
    issuer.set_real_ip(it->value().to_string());
  }
  it = req.find("X-Forwarded-For");
  if (it != req.end()) {
    issuer.set_forwarded_for(it->value().to_string());
  }
  issuer.set_path_info(req.target().to_string());
  issuer.set_github_username(std::move(github_user));
  return issuer;
}

static wandbox::cattleshed::RunJobRequest make_run_job_request(
    const wandbox::kennel::CompileParameter& req,
    wandbox::cattleshed::Issuer issuer) {
  wandbox::cattleshed::RunJobRequest request;
  auto start = request.mutable_start();
  start->set_compiler(req.compiler);
  start->set_stdin(req.stdin);
  start->set_compiler_option_raw(req.compiler_option_raw);
  start->set_runtime_option_raw(req.runtime_option_raw);
  start->set_default_source(req.code);
  start->set_compiler_options(req.options);
  *start->mutable_issuer() = std::move(issuer);
  for (const auto& code : req.codes) {
    auto source = start->add_sources();
    source->set_file_name(code.file);
    source->set_source(code.code);
  }
  return request;
}

struct sponsor {
  std::string name;
  std::string url;
  std::time_t due_date;
};

std::time_t from_iso8601(std::string str) {
  std::tm tm;
  std::memset(&tm, 0, sizeof(tm));
  strptime(str.c_str(), "%FT%T%z", &tm);
  return std::mktime(&tm);
}

static std::string make_random_name(std::size_t length) {
  std::string name;
  std::random_device seed_gen;
  std::mt19937 engine(seed_gen());
  std::uniform_int_distribution<> dist(0, 127);
  while (name.size() < length) {
    auto c = (char)dist(engine);
    if (('0' <= c && c <= '9') || ('a' <= c && c <= 'z') ||
        ('A' <= c && c <= 'Z')) {
      name.push_back(c);
    }
  }
  return name;
}

static wandbox::kennel::PostPermlinkRequest make_permlink_request(
    const wandbox::kennel::CompileParameter& kreq,
    std::vector<wandbox::kennel::CompileNdjsonResult> results) {
  wandbox::kennel::PostPermlinkRequest preq;
  preq.title = kreq.title;
  preq.description = kreq.description;
  preq.compiler = kreq.compiler;
  preq.code = kreq.code;
  preq.codes = kreq.codes;
  preq.options = kreq.options;
  preq.stdin = kreq.stdin;
  preq.compiler_option_raw = kreq.compiler_option_raw;
  preq.runtime_option_raw = kreq.runtime_option_raw;
  preq.results = std::move(results);
  return preq;
}

// 長さチェック
static bool check_title(const wandbox::kennel::CompileParameter& p) {
  // ブラウザでの制限がコードポイント単位で 100 なので、
  // UTF-8 換算で適当に 400 あたりにしておく。
  if (p.title.size() > 400) {
    return false;
  }
  // こっちはコードポイント単位で 1000
  if (p.description.size() > 4000) {
    return false;
  }
  return true;
}
static bool check_title(const wandbox::kennel::PostPermlinkRequest& p) {
  if (p.title.size() > 400) {
    return false;
  }
  if (p.description.size() > 4000) {
    return false;
  }
  return true;
}

struct KennelSessionConfig {
  std::shared_ptr<CattleshedClientManager> cm;
  std::shared_ptr<KennelSharedData> sd;
  std::shared_ptr<wandbox::kennel::SponsorFile> sponsor_file;
  std::string database;
  std::string url;
};

// HTTP の１回のリクエストに対して答えるためのクラス
class KennelSession : public std::enable_shared_from_this<KennelSession> {
  KennelSession(boost::asio::ip::tcp::socket socket, KennelSessionConfig config)
      : socket_(std::move(socket)), config_(std::move(config)) {}

 public:
  static std::shared_ptr<KennelSession> Create(
      boost::asio::ip::tcp::socket socket, KennelSessionConfig config) {
    return std::shared_ptr<KennelSession>(
        new KennelSession(std::move(socket), std::move(config)));
  }

  void Run() { DoRead(); }

 private:
  void DoRead() {
    // Make the request empty before reading,
    // otherwise the operation behavior is undefined.
    req_ = {};

    // Read a request
    boost::beast::http::async_read(
        socket_, buffer_, req_,
        std::bind(&KennelSession::OnRead, shared_from_this(),
                  std::placeholders::_1, std::placeholders::_2));
  }

  void OnRead(boost::system::error_code ec, std::size_t bytes_transferred) {
    boost::ignore_unused(bytes_transferred);

    // This means they closed the connection
    if (ec == boost::beast::http::error::end_of_stream) {
      DoClose();
      return;
    }

    if (ec) {
      SPDLOG_ERROR("Failed to read: {}", ec.message());
      return;
    }

    SPDLOG_DEBUG("[{}] requested", req_.target().to_string());

    if (req_.method() == boost::beast::http::verb::get) {
      if (req_.target() == "/api/sponsors.json") {
        HandleGetSponsors();
      } else if (req_.target() == "/api/list.json") {
        HandleGetList();
      } else if (req_.target().starts_with("/api/permlink/")) {
        HandleGetPermlink(req_.target().substr(sizeof("/api/permlink/") - 1));
      } else if (req_.target().starts_with("/api/template/")) {
        HandleGetTemplate(req_.target().substr(sizeof("/api/template/") - 1));
      } else {
        SendResponse(NotFound(req_, req_.target()));
      }
    } else if (req_.method() == boost::beast::http::verb::post) {
      if (req_.target() == "/api/compile.json") {
        HandlePostCompileJson();
      } else if (req_.target() == "/api/compile.ndjson") {
        HandlePostCompileNdjson();
      } else if (req_.target() == "/api/permlink") {
        HandlePostPermlink();
      } else {
        SendResponse(NotFound(req_, req_.target()));
      }
    } else {
      SendResponse(NotFound(req_, req_.target()));
    }
  }

  void HandleGetSponsors() {
    // 期限切れのを除きつつ SponsorFile を SponsorResponse に変換する
    wandbox::kennel::SponsorResponse resp;
    auto now = std::time(nullptr);
    for (const auto& s : config_.sponsor_file->corporate) {
      wandbox::kennel::TimestampSponsor ts;
      ts.name = s.name;
      ts.url = s.url;
      ts.due_date = from_iso8601(s.due_date);
      if (now <= ts.due_date) {
        resp.corporate.push_back(std::move(ts));
      }
    }
    for (const auto& s : config_.sponsor_file->personal) {
      wandbox::kennel::TimestampSponsor ts;
      ts.name = s.name;
      ts.url = s.url;
      ts.due_date = from_iso8601(s.due_date);
      if (now <= ts.due_date) {
        resp.personal.push_back(std::move(ts));
      }
    }

    SendResponse(CreateOKWithJSON(req_, boost::json::value_from(resp)));
  }

  void HandleGetList() {
    config_.sd->MaybeUpdateCattleshedInfo();
    auto info = config_.sd->GetCattleshedInfo();

    auto resp =
        CreateOKWithJSON(req_, boost::json::value_from(info->compilers));
    SendResponse(resp);
  }

  void HandleGetPermlink(boost::beast::string_view permlink_id) {
    permlink pl(config_.database);
    auto presp = pl.get_permlink(permlink_id.to_string());
    for (const auto& r : presp.results) {
      wandbox::cattleshed::RunJobResponse cr;
      cr.set_type(string_to_response_type(r.type));
      cr.set_data(r.data);
      update_compile_result(presp.result, cr);
    }

    auto resp = CreateOKWithJSON(req_, boost::json::value_from(presp));
    SendResponse(resp);
  }

  void HandleGetTemplate(boost::beast::string_view templateName) {
    const auto& tmpls = config_.sd->GetCattleshedInfo()->templates;
    auto it = std::find_if(tmpls.begin(), tmpls.end(),
                           [&templateName](const auto& tmpl) {
                             return tmpl.name == templateName;
                           });
    if (it == tmpls.end()) {
      SendResponse(NotFound(req_, req_.target()));
      return;
    }

    auto resp = CreateOKWithJSON(req_, boost::json::value_from(*it));
    SendResponse(resp);
  }

  void HandlePostCompileJson() {
    auto kreq =
        jsonif::from_json<wandbox::kennel::CompileParameter>(req_.body());
    if (!check_title(kreq)) {
      SendResponse(BadRequest(req_, "Too long title or description"));
      return;
    }
    const auto& compilers = config_.sd->GetCattleshedInfo()->compilers;
    auto info = std::find_if(
        compilers.begin(), compilers.end(),
        [&kreq](const auto& c) { return c.name == kreq.compiler; });
    if (info == compilers.end()) {
      SendResponse(NotFound(req_, kreq.compiler));
      return;
    }
    auto issuer = make_issuer(req_, kreq.github_user);
    auto creq = make_run_job_request(kreq, std::move(issuer));
    auto client = config_.cm->CreateRunJobClient();
    auto result = std::make_shared<wandbox::kennel::CompileResult>();
    auto results =
        std::make_shared<std::vector<wandbox::kennel::CompileNdjsonResult>>();
    client->SetOnRead([self = shared_from_this(), result,
                       results](wandbox::cattleshed::RunJobResponse resp) {
      SPDLOG_TRACE("[/compiler.ndjson] OnRead: {}", resp.DebugString());
      boost::asio::post(self->socket_.get_executor(),
                        [self, resp = std::move(resp), result, results]() {
                          update_compile_result(*result, resp);

                          wandbox::kennel::CompileNdjsonResult r;
                          r.type = response_type_to_string(resp.type());
                          r.data = resp.data();
                          results->push_back(std::move(r));
                        });
    });
    client->SetOnFinish([self = shared_from_this(), client, result, kreq,
                         results, compiler_info = *info](grpc::Status status) {
      SPDLOG_TRACE("[/compiler.ndjson] OnFinish");
      boost::asio::post(
          self->socket_.get_executor(),
          [self, status, result, kreq, results, compiler_info]() {
            if (kreq.save) {
              permlink pl(self->config_.database);
              std::string permlink_id = make_random_name(16);
              auto preq =
                  make_permlink_request(std::move(kreq), std::move(*results));
              pl.make_permlink(permlink_id, std::move(preq), compiler_info);
              result->permlink = permlink_id;
              result->url =
                  fmt::format("{}/permlink/{}", self->config_.url, permlink_id);
            }

            auto resp =
                CreateOKWithJSON(self->req_, boost::json::value_from(*result));
            self->SendResponse(resp);
          });
      client->Close();
    });
    client->Connect();
    client->Write(std::move(creq));
    client->WritesDone();
  }

  void HandlePostCompileNdjson() {
    auto kreq =
        jsonif::from_json<wandbox::kennel::CompileParameter>(req_.body());
    if (!check_title(kreq)) {
      SendResponse(BadRequest(req_, "Too long title or description"));
      return;
    }
    const auto& compilers = config_.sd->GetCattleshedInfo()->compilers;
    auto info = std::find_if(
        compilers.begin(), compilers.end(),
        [&kreq](const auto& c) { return c.name == kreq.compiler; });
    if (info == compilers.end()) {
      SendResponse(NotFound(req_, kreq.compiler));
      return;
    }
    auto issuer = make_issuer(req_, kreq.github_user);
    auto creq = make_run_job_request(kreq, std::move(issuer));
    auto client = config_.cm->CreateRunJobClient();
    client->SetOnRead([self = shared_from_this()](
                          wandbox::cattleshed::RunJobResponse resp) {
      SPDLOG_TRACE("[client][/compiler.ndjson] OnRead: {}", resp.DebugString());
      boost::asio::post(self->socket_.get_executor(),
                        [self, resp = std::move(resp)]() {
                          wandbox::kennel::CompileNdjsonResult r;
                          r.type = response_type_to_string(resp.type());
                          r.data = resp.data();
                          self->SendChunk(jsonif::to_json(r) + "\n");
                        });
    });
    client->SetOnFinish(
        [self = shared_from_this(), client, kreq](grpc::Status status) {
          SPDLOG_TRACE("[client][/compiler.ndjson] OnFinish");
          boost::asio::post(self->socket_.get_executor(),
                            [self, status]() { self->SendChunk(""); });
          client->Close();
        });
    client->Connect();
    client->Write(std::move(creq));
    client->WritesDone();

    boost::beast::http::response<boost::beast::http::empty_body> header;
    header.set(boost::beast::http::field::content_type, "application/json");
    SendHeader(header);
  }

  void HandlePostPermlink() {
    auto preq =
        jsonif::from_json<wandbox::kennel::PostPermlinkRequest>(req_.body());
    if (!check_title(preq)) {
      SendResponse(BadRequest(req_, "Too long title or description"));
      return;
    }
    const auto& compilers = config_.sd->GetCattleshedInfo()->compilers;
    auto info = std::find_if(
        compilers.begin(), compilers.end(),
        [&preq](const auto& c) { return c.name == preq.compiler; });
    if (info == compilers.end()) {
      SendResponse(NotFound(req_, preq.compiler));
      return;
    }

    permlink pl(config_.database);
    std::string permlink_id = make_random_name(16);
    pl.make_permlink(permlink_id, std::move(preq), *info);

    wandbox::kennel::PostPermlinkResponse presp;
    presp.permlink = permlink_id;
    presp.url = fmt::format("{}/permlink/{}", config_.url, permlink_id);
    auto resp = CreateOKWithJSON(req_, boost::json::value_from(presp));
    SendResponse(resp);
  }

  void OnWrite(boost::system::error_code ec, std::size_t bytes_transferred,
               bool close) {
    boost::ignore_unused(bytes_transferred);

    if (ec) {
      SPDLOG_ERROR("Failed to write: {}", ec.message());
    }

    if (close) {
      DoClose();
      return;
    }

    res_ = nullptr;
    res2_ = nullptr;

    DoRead();
  }

  void OnWriteHeader(boost::system::error_code ec,
                     std::size_t bytes_transferred) {
    if (chunk_state_ != ChunkState::SendingHeader) {
      SPDLOG_ERROR("Invalid state: {}", (int)chunk_state_);
      return;
    }

    ProcessNextChunk();
  }

  void OnWriteChunk(boost::system::error_code ec,
                    std::size_t bytes_transferred) {
    if (chunk_queue_.empty()) {
      SPDLOG_ERROR("Invalid chunk queue");
      return;
    }
    chunk_queue_.pop();

    if (chunk_state_ != ChunkState::SendingChunk) {
      SPDLOG_ERROR("Invalid state: {}", (int)chunk_state_);
      return;
    }

    ProcessNextChunk();
  }

  void OnWriteLastChunk(boost::system::error_code ec,
                        std::size_t bytes_transferred) {
    if (chunk_queue_.empty()) {
      SPDLOG_ERROR("Invalid chunk queue");
      return;
    }
    chunk_queue_.pop();

    if (chunk_state_ != ChunkState::SendingLastChunk) {
      SPDLOG_ERROR("Invalid state: {}", (int)chunk_state_);
      return;
    }

    chunk_state_ = ChunkState::Completed;

    DoClose();
  }

  void DoClose() {
    // Send a TCP shutdown
    boost::system::error_code ec;
    socket_.shutdown(boost::asio::ip::tcp::socket::shutdown_send, ec);

    // At this point the connection is closed gracefully
  }

 private:
  static boost::beast::http::response<boost::beast::http::string_body>
  CreateOKWithJSON(
      const boost::beast::http::request<boost::beast::http::string_body>& req,
      boost::json::value json_message) {
    boost::beast::http::response<boost::beast::http::string_body> res{
        boost::beast::http::status::ok, 11};
    res.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
    res.set(boost::beast::http::field::content_type, "application/json");
    res.keep_alive(req.keep_alive());
    res.body() = boost::json::serialize(json_message);
    res.prepare_payload();

    return res;
  }

  template <class Body, class Fields>
  void SendResponse(boost::beast::http::response<Body, Fields> msg) {
    auto sp = std::make_shared<boost::beast::http::response<Body, Fields>>(
        std::move(msg));

    // msg オブジェクトは書き込みが完了するまで生きている必要があるので、
    // メンバに入れてライフタイムを延ばしてやる
    res_ = sp;

    // Write the response
    boost::beast::http::async_write(
        socket_, *sp,
        std::bind(&KennelSession::OnWrite, shared_from_this(),
                  std::placeholders::_1, std::placeholders::_2,
                  sp->need_eof()));

    SPDLOG_DEBUG("[{}] responsed", req_.target().to_string());
  }

  enum class ChunkState {
    Init,
    SendingHeader,
    Idle,
    SendingChunk,
    SendingLastChunk,
    Completed,
  };

  // ストリーミング送信の開始
  void SendHeader(
      boost::beast::http::response<boost::beast::http::empty_body> header) {
    if (chunk_state_ != ChunkState::Init) {
      SPDLOG_ERROR("Invalid State {}", (int)chunk_state_);
      return;
    }

    header.chunked(true);
    auto sp = std::make_shared<
        boost::beast::http::response<boost::beast::http::empty_body>>(
        std::move(header));
    auto sp2 = std::make_shared<boost::beast::http::response_serializer<
        boost::beast::http::empty_body>>(*sp);

    res_ = sp;
    res2_ = sp2;

    chunk_state_ = ChunkState::SendingHeader;
    boost::beast::http::async_write_header(
        socket_, *sp2,
        std::bind(&KennelSession::OnWriteHeader, shared_from_this(),
                  std::placeholders::_1, std::placeholders::_2));
  }

  // ストリーミングデータの送信
  // 空文字だったら終端扱いになる
  void SendChunk(std::string data) {
    if (chunk_state_ != ChunkState::SendingChunk &&
        chunk_state_ != ChunkState::Idle) {
      SPDLOG_ERROR("Invalid State {}", (int)chunk_state_);
      return;
    }

    chunk_queue_.push(std::make_unique<std::string>(std::move(data)));

    if (chunk_state_ == ChunkState::SendingChunk) {
      return;
    }

    ProcessNextChunk();
  }

  // キューの先頭のデータを送信する
  void ProcessNextChunk() {
    if (chunk_queue_.empty()) {
      chunk_state_ = ChunkState::Idle;
      return;
    }

    const std::unique_ptr<std::string>& data = chunk_queue_.front();
    if (data->empty()) {
      // 終端
      auto chunk = boost::beast::http::make_chunk_last();

      chunk_state_ = ChunkState::SendingLastChunk;
      boost::beast::net::async_write(
          socket_, chunk,
          std::bind(&KennelSession::OnWriteLastChunk, shared_from_this(),
                    std::placeholders::_1, std::placeholders::_2));
    } else {
      auto chunk = boost::beast::http::make_chunk(boost::asio::buffer(*data));

      chunk_state_ = ChunkState::SendingChunk;
      boost::beast::net::async_write(
          socket_, chunk,
          std::bind(&KennelSession::OnWriteChunk, shared_from_this(),
                    std::placeholders::_1, std::placeholders::_2));
    }
  }

 private:
  boost::asio::ip::tcp::socket socket_;
  boost::beast::flat_buffer buffer_;
  boost::beast::http::request<boost::beast::http::string_body> req_;
  std::shared_ptr<void> res_;
  std::shared_ptr<void> res2_;
  ChunkState chunk_state_ = ChunkState::Init;
  // push 時の再配置で std::string がコピーされると困るので unique_ptr で保持する
  std::queue<std::unique_ptr<std::string>> chunk_queue_;

  KennelSessionConfig config_;
};

struct KennelServerConfig {
  boost::asio::ip::tcp::endpoint endpoint;
  std::shared_ptr<CattleshedClientManager> cm;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> initial_cattleshed_info;
  std::shared_ptr<wandbox::kennel::SponsorFile> sponsor_file;
  std::string database;
  std::string url;
};

class KennelServer : public std::enable_shared_from_this<KennelServer> {
  KennelServer(boost::asio::io_context& ioc, KennelServerConfig config)
      : acceptor_(ioc), socket_(ioc), config_(std::move(config)) {
    boost::system::error_code ec;

    // Open the acceptor
    acceptor_.open(config_.endpoint.protocol(), ec);
    if (ec) {
      SPDLOG_ERROR("Failed to open: {}", ec.message());
      return;
    }

    // Allow address reuse
    acceptor_.set_option(boost::asio::socket_base::reuse_address(true), ec);
    if (ec) {
      SPDLOG_ERROR("Failed to set_option: {}", ec.message());
      return;
    }

    // Bind to the server address
    acceptor_.bind(config_.endpoint, ec);
    if (ec) {
      SPDLOG_ERROR("Failed to bind: {}", ec.message());
      return;
    }

    // Start listening for connections
    acceptor_.listen(boost::asio::socket_base::max_listen_connections, ec);
    if (ec) {
      SPDLOG_ERROR("Failed to listen: {}", ec.message());
      return;
    }

    KennelSharedDataConfig sdconfig;
    sdconfig.cm = config_.cm;
    sdconfig.ioc = &ioc;
    sdconfig.ioc = &ioc;
    sdconfig.initial_cattleshed_info = config_.initial_cattleshed_info;
    shared_data_.reset(new KennelSharedData(std::move(sdconfig)));
    initialized_ = true;
  }

 public:
  static std::shared_ptr<KennelServer> Create(boost::asio::io_context& ioc,
                                              KennelServerConfig config) {
    auto p =
        std::shared_ptr<KennelServer>(new KennelServer(ioc, std::move(config)));
    if (!p->initialized_) {
      return nullptr;
    }

    return p;
  }

  void Run() { DoAccept(); }

 private:
  void DoAccept() {
    acceptor_.async_accept(
        socket_, std::bind(&KennelServer::OnAccept, shared_from_this(),
                           std::placeholders::_1));
  }
  void OnAccept(boost::system::error_code ec) {
    if (ec) {
      SPDLOG_ERROR("Failed to accept: {}", ec.message());
    } else {
      KennelSessionConfig config;
      config.cm = config_.cm;
      config.sd = shared_data_;
      config.sponsor_file = config_.sponsor_file;
      config.database = config_.database;
      config.url = config_.url;
      KennelSession::Create(std::move(socket_), std::move(config))->Run();
    }

    DoAccept();
  }

 private:
  boost::asio::ip::tcp::acceptor acceptor_;
  boost::asio::ip::tcp::socket socket_;

  KennelServerConfig config_;
  std::shared_ptr<KennelSharedData> shared_data_;
  bool initialized_ = false;
};

int main(int argc, char* argv[]) {
  CLI::App app("Kennel - Wandbox API Server");

  std::string host = "127.0.0.1";
  int port = 3500;
  std::string cattleshed_host = "127.0.0.1";
  int cattleshed_port = 50051;
  std::string sponsor_json = "./sponsors.json";
  std::string database = "sqlite3:db=kennel.sqlite;@pool_size=10";
  std::string url = "http://localhost:8787";
  int log_level = spdlog::level::info;

  auto log_level_map = std::vector<std::pair<std::string, int>>(
      {{"trace", spdlog::level::trace},
       {"debug", spdlog::level::debug},
       {"warning", spdlog::level::warn},
       {"error", spdlog::level::err},
       {"critical", spdlog::level::critical},
       {"off", spdlog::level::off}});

  app.add_option("--host", host, "Listen host");
  app.add_option("--port", port, "Listen port");
  app.add_option("--cattleshed-host", cattleshed_host, "Cattleshed host");
  app.add_option("--cattleshed-port", cattleshed_port, "Cattleshed port");
  app.add_option("--sponsorsfile", sponsor_json, "Sponsors file");
  app.add_option("--database", database, "Database URI");
  app.add_option("--url", url, "Public URL for Wandbox");
  app.add_option("--log-level", log_level, "Log severity level threshold")
      ->transform(CLI::CheckedTransformer(log_level_map, CLI::ignore_case));

  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    exit(app.exit(e));
  }

  spdlog::set_level((spdlog::level::level_enum)log_level);

  wandbox::kennel::SponsorFile sponsor_file;
  if (sponsor_json.empty()) {
    SPDLOG_INFO("Sponsor file not set");
  } else {
    std::ifstream ifs(sponsor_json.c_str());
    if (!ifs) {
      SPDLOG_ERROR("Sponsor file {} not found", sponsor_json);
      return -1;
    }

    std::stringstream ss;
    ss << ifs.rdbuf();

    try {
      sponsor_file = jsonif::from_json<wandbox::kennel::SponsorFile>(ss.str());
    } catch (...) {
      SPDLOG_ERROR("Sponsor file {} parse error", sponsor_json);
      return -1;
    }
    SPDLOG_INFO("Sponsor file {} load completed", sponsor_json);
  }

  auto channel = grpc::CreateChannel(
      cattleshed_host + ":" + std::to_string(cattleshed_port),
      grpc::InsecureChannelCredentials());
  SPDLOG_INFO("Create gRPC channel to {}",
              cattleshed_host + ":" + std::to_string(cattleshed_port));
  std::shared_ptr<CattleshedClientManager> cm(
      new CattleshedClientManager(channel, 1));

  // DB の初期化
  permlink pl(database);
  pl.init();

  // cattleshed の GetVersion を呼んで、無事結果を取得できたらサーバーを起動する
  std::shared_ptr<wandbox::kennel::CattleshedInfo> info;
  {
    auto promise =
        std::make_shared<std::promise<wandbox::kennel::CattleshedInfo>>();
    auto future = promise->get_future();

    auto client = cm->CreateGetVersionClient();
    client->SetOnFinish(
        [client, promise](wandbox::cattleshed::GetVersionResponse resp,
                          grpc::Status status) mutable {
          if (!status.ok()) {
            SPDLOG_ERROR("Failed to GetVersion: {}, {}",
                         (int)status.error_code(), status.error_message());
            promise->set_exception(std::make_exception_ptr("error"));
            return;
          }
          promise->set_value(version_response_to_kennel(resp));
        });
    client->SetOnError(
        [promise](ggrpc::ClientResponseReaderError error) mutable {
          SPDLOG_ERROR("Failed to GetVersion (OnError): {}", (int)error);
          promise->set_exception(std::make_exception_ptr("error"));
        });
    wandbox::cattleshed::GetVersionRequest req;
    client->Connect(req);
    auto status = future.wait_for(std::chrono::seconds(60));
    if (status != std::future_status::ready) {
      SPDLOG_ERROR("Failed to Wait GetVersion");
      return -1;
    }
    info.reset(new wandbox::kennel::CattleshedInfo(future.get()));
  }

  boost::asio::io_context ioc{1};
  KennelServerConfig config;
  config.endpoint = boost::asio::ip::tcp::endpoint(
      boost::asio::ip::make_address(host), static_cast<unsigned short>(port));
  config.cm = cm;
  config.initial_cattleshed_info = info;
  config.sponsor_file =
      std::make_shared<wandbox::kennel::SponsorFile>(std::move(sponsor_file));
  config.database = std::move(database);
  config.url = std::move(url);
  KennelServer::Create(ioc, std::move(config))->Run();
  ioc.run();
}