#ifndef KENNEL_SESSION_H_
#define KENNEL_SESSION_H_

#include <memory>
#include <string>
#include <vector>
#include <queue>

// spdlog
#include <spdlog/spdlog.h>

// Boost
#include <boost/asio.hpp>
#include <boost/beast.hpp>
#include <boost/json.hpp>

#include "cattleshed_client.h"
#include "kennel.json.h"
#include "permlink.h"
#include "kennel_shared_data.h"
#include "jsonif_nothrow.h"
#include "jsonif_serializer.h"

boost::beast::http::response<boost::beast::http::string_body> BadRequest(
    const boost::beast::http::request<boost::beast::http::string_body>& req,
    boost::beast::string_view why) {
  boost::beast::http::response<boost::beast::http::string_body> res{
      boost::beast::http::status::bad_request, req.version()};
  res.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
  res.set(boost::beast::http::field::content_type, "text/html");
  res.keep_alive(req.keep_alive());
  res.body() = why;
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
  res.body() = "The resource '" + std::string(target) + "' was not found.";
  res.prepare_payload();
  return res;
}

boost::beast::http::response<boost::beast::http::string_body> ServerError(
    const boost::beast::http::request<boost::beast::http::string_body>& req,
    boost::beast::string_view what) {
  boost::beast::http::response<boost::beast::http::string_body> res{
      boost::beast::http::status::internal_server_error, req.version()};
  res.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
  res.set(boost::beast::http::field::content_type, "text/html");
  res.keep_alive(req.keep_alive());
  res.body() = "An error occurred: '" + std::string(what) + "'";
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
    issuer.set_real_ip(std::string(it->value()));
  }
  it = req.find("X-Forwarded-For");
  if (it != req.end()) {
    issuer.set_forwarded_for(std::string(it->value()));
  }
  issuer.set_path_info(std::string(req.target()));
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
  std::shared_ptr<std::string> hpplib_json;
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

    SPDLOG_DEBUG("[{}] requested", std::string(req_.target()));

    // 念のため catch しておく（投げっぱなしだとプロセスごと落ちてしまうので）
    try {
      if (req_.method() == boost::beast::http::verb::get) {
        if (req_.target() == "/api/sponsors.json") {
          HandleGetSponsors();
        } else if (req_.target() == "/api/list.json") {
          HandleGetList();
        } else if (req_.target() == "/api/hpplib.json") {
          HandleGetHpplib();
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
    } catch (const char* e) {
      SPDLOG_ERROR("[{}] Unexpected exception: {}", std::string(req_.target()),
                   e);
      SendResponse(ServerError(req_, e));
    } catch (std::exception& e) {
      SPDLOG_ERROR("[{}] Unexpected exception: {}", std::string(req_.target()),
                   e.what());
      SendResponse(ServerError(req_, e.what()));
    } catch (...) {
      SPDLOG_ERROR("[{}] Unexpected exception", std::string(req_.target()));
      SendResponse(ServerError(req_, "unexpected"));
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

  void HandleGetHpplib() {
    std::string contents;
    std::ifstream ifs(config_.hpplib_json->c_str());
    if (!ifs) {
      contents = "[]";
    } else {
      std::stringstream ss;
      ss << ifs.rdbuf();
      contents = ss.str();
    }
    boost::beast::http::response<boost::beast::http::string_body> resp{
        boost::beast::http::status::ok, 11};
    resp.set(boost::beast::http::field::server, BOOST_BEAST_VERSION_STRING);
    resp.set(boost::beast::http::field::content_type, "application/json");
    resp.keep_alive(req_.keep_alive());
    resp.body() = contents;
    resp.prepare_payload();
    SendResponse(resp);
  }

  void HandleGetPermlink(boost::beast::string_view permlink_id) {
    permlink pl(config_.database);
    wandbox::kennel::GetPermlinkResponse presp;
    try {
      presp = pl.get_permlink(permlink_id);
    } catch (cppdb::null_value_fetch) {
      SendResponse(NotFound(req_, req_.target()));
      return;
    }

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
    std::exception_ptr ep;
    auto kreq =
        jsonif::from_json<wandbox::kennel::CompileParameter>(req_.body(), ep);
    if (ep) {
      SendResponse(BadRequest(req_, "Invalid JSON"));
      return;
    }
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
    std::exception_ptr ep;
    auto kreq =
        jsonif::from_json<wandbox::kennel::CompileParameter>(req_.body(), ep);
    if (ep) {
      SendResponse(BadRequest(req_, "Invalid JSON"));
      return;
    }
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
    std::exception_ptr ep;
    auto preq = jsonif::from_json<wandbox::kennel::PostPermlinkRequest>(
        req_.body(), ep);
    if (ep) {
      SendResponse(BadRequest(req_, "Invalid JSON"));
      return;
    }
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

    SPDLOG_INFO("[{}] responsed {}", std::string(req_.target()),
                (int)sp->result());
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

#endif // KENNEL_SESSION_H_