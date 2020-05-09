#ifndef CATTLESHED_SERVER_H_INCLUDED
#define CATTLESHED_SERVER_H_INCLUDED

#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <thread>
#include <tuple>

// Linux
#include <aio.h>
#include <locale.h>
#include <sys/eventfd.h>
#include <time.h>

// ggrpc
#include <ggrpc/server.h>

// spdlog
#include <spdlog/spdlog.h>

// protobuf
#include <google/protobuf/util/json_util.h>

#include "cattleshed.grpc.pb.h"
#include "cattleshed.pb.h"
#include "load_config.hpp"
#include "posixapi.hpp"

class GetVersionHandler
    : public ggrpc::ServerResponseWriterHandler<cattleshed::GetVersionResponse,
                                                cattleshed::GetVersionRequest> {
 public:
  GetVersionHandler(cattleshed::Cattleshed::AsyncService* service,
                    std::shared_ptr<boost::asio::io_context> ioc,
                    std::shared_ptr<boost::asio::signal_set> sigs,
                    const wandbox::server_config* config)
      : service_(service), ioc_(ioc), sigs_(sigs), config_(config) {}

 private:
  struct VersionRunner {
    VersionRunner(std::shared_ptr<boost::asio::io_context> ioc,
                  std::shared_ptr<boost::asio::signal_set> sigs,
                  const wandbox::server_config& config)
        : ioc_(ioc), sigs_(sigs), config_(&config) {
      for (const wandbox::compiler_trait& c : config_->compilers) {
        commands_.push_back(c);
      }
    }
    void AsyncRun(std::function<
                  void(boost::system::error_code ec,
                       const std::vector<std::pair<std::string, std::string>>&)>
                      handler) {
      // 全部のバージョン取得処理が終わったので結果を返す
      if (commands_.empty()) {
        handler(boost::system::error_code(), versions_);
        handler = nullptr;
        return;
      }

      current_ = commands_.front();
      commands_.pop_front();
      if (current_.version_command.empty() || not current_.displayable) {
        // 次へ
        AsyncRun(std::move(handler));
        return;
      }

      // 実行開始
      {
        auto c = wandbox::piped_spawn(wandbox::opendir("/"),
                                      current_.version_command);
        SPDLOG_DEBUG("[0x{}] run [{}]", (void*)this,
                     boost::algorithm::join(current_.version_command, " "));
        child_ = std::make_shared<wandbox::unique_child_pid>(std::move(c.pid));
        pipe_stdout_ = std::make_shared<boost::asio::posix::stream_descriptor>(
            *ioc_, c.fd_stdout.get());
        c.fd_stdout.release();
      }

      // 実行完了を待つ
      sigs_->async_wait(std::bind(&VersionRunner::OnWait, this,
                                  std::placeholders::_1,
                                  std::placeholders::_2));

      handler_ = std::move(handler);
    }

   private:
    void OnWait(const boost::system::error_code& ec, int signum) {
      child_->wait_nonblock();
      if (not child_->finished()) {
        // まだ終わってないので再度待つ
        sigs_->async_wait(std::bind(&VersionRunner::OnWait, this,
                                    std::placeholders::_1,
                                    std::placeholders::_2));
        return;
      }

      {
        int st = child_->wait_nonblock();
        SPDLOG_DEBUG("[0x{}] WIFEXITED(st): {}, WEXITSTATUS(st): {}",
                     (void*)this, WIFEXITED(st), WEXITSTATUS(st));
        // バージョン取得に失敗したので次へ
        if (!WIFEXITED(st) || (WEXITSTATUS(st) != 0)) {
          AsyncRun(std::move(handler_));
          return;
        }
      }

      // 出力の読み込み
      DoRead();
    }

    void DoRead() {
      buf_ = std::make_shared<boost::asio::streambuf>();
      boost::asio::async_read_until(
          *pipe_stdout_, *buf_, '\n',
          std::bind(&VersionRunner::OnRead, this, std::placeholders::_1,
                    std::placeholders::_2));
    }

    void OnRead(boost::system::error_code ec, std::size_t bytes_transferred) {
      // なぜか失敗したので次へ
      if (ec) {
        SPDLOG_WARN("[0x{}] failed to async_read_until: {}", (void*)this,
                    ec.message());
        AsyncRun(std::move(handler_));
        return;
      }

      std::istream is(buf_.get());
      std::string ver;
      if (!std::getline(is, ver)) {
        SPDLOG_WARN("[0x{}] failed to getline", (void*)this);
        AsyncRun(std::move(handler_));
        return;
      }

      SPDLOG_DEBUG("[0x{}] add version: {} {}", (void*)this, current_.name,
                   ver);
      versions_.push_back(std::make_pair(current_.name, ver));

      // 無事バージョンの取得に成功したので次へ
      AsyncRun(std::move(handler_));
    }

   private:
    std::shared_ptr<boost::asio::io_context> ioc_;
    std::shared_ptr<boost::asio::signal_set> sigs_;
    const wandbox::server_config* config_;

    wandbox::compiler_trait current_;
    std::deque<wandbox::compiler_trait> commands_;
    std::function<void(boost::system::error_code ec,
                       const std::vector<std::pair<std::string, std::string>>&)>
        handler_;
    std::vector<std::pair<std::string, std::string>> versions_;
    std::shared_ptr<boost::asio::posix::stream_descriptor> pipe_stdout_;
    std::shared_ptr<wandbox::unique_child_pid> child_;
    std::shared_ptr<boost::asio::streambuf> buf_;
  };

  void Request(grpc::ServerContext* context,
               cattleshed::GetVersionRequest* request,
               grpc::ServerAsyncResponseWriter<cattleshed::GetVersionResponse>*
                   response_writer,
               grpc::ServerCompletionQueue* cq, void* tag) override {
    service_->RequestGetVersion(context, request, response_writer, cq, cq, tag);
  }
  void OnAccept(cattleshed::GetVersionRequest request) override {
    version_runner_.reset(new VersionRunner(ioc_, sigs_, *config_));
    version_runner_->AsyncRun(
        [this, context = Context()](
            boost::system::error_code ec,
            const std::vector<std::pair<std::string, std::string>>& versions) {
          // バージョンが取得できたので、レスポンス用データを作る
          auto resp = GenResponse(*config_, versions);

          context->Finish(std::move(resp), grpc::Status::OK);
        });
  }

  static cattleshed::GetVersionResponse GenResponse(
      const wandbox::server_config& config,
      const std::vector<std::pair<std::string, std::string>>& versions) {
    cattleshed::GetVersionResponse resp;

    for (const auto& pair : versions) {
      const auto it = config.compilers.get<1>().find(pair.first);
      // 必ずあるはずなんだけど一応チェック
      if (it == config.compilers.get<1>().end()) {
        continue;
      }
      const wandbox::compiler_trait& compiler = *it;

      cattleshed::CompilerInfo* info = resp.add_compiler_info();

      for (const auto& sw : compiler.local_switches.get<1>()) {
        auto rng = compiler.local_switches.get<2>().equal_range(sw.group_name);
        if (rng.first->name != sw.name) {
          continue;
        }

        if (std::next(rng.first) != rng.second) {
          cattleshed::SelectSwitch* sel =
              info->add_switches()->mutable_select();
          std::string def;
          for (; rng.first != rng.second; ++rng.first) {
            cattleshed::SelectSwitchOption* opt = sel->add_options();
            auto&& sw = *rng.first;
            opt->set_name(sw.name);
            opt->set_display_name(sw.display_name);
            opt->set_display_flags(sw.display_flags
                                       ? *sw.display_flags
                                       : boost::algorithm::join(sw.flags, " "));

            if (compiler.initial_checked.count(sw.name) != 0) {
              def = sw.name;
            }
          }
          sel->set_name(sw.group_name);
          sel->set_default_value(def);
        } else {
          cattleshed::SingleSwitch* single =
              info->add_switches()->mutable_single();
          auto&& sw = *rng.first;
          single->set_name(sw.name);
          single->set_display_name(sw.display_name);
          single->set_default_value(compiler.initial_checked.count(sw.name) !=
                                    0);
        }
      }

      std::unordered_set<std::string> used;
      for (const auto& swname : compiler.switches) {
        if (used.count(swname) != 0) {
          continue;
        }
        if (compiler.local_switches.get<1>().count(swname)) {
          continue;
        }
        const auto ite = config.switches.find(swname);
        if (ite == config.switches.end()) {
          continue;
        }
        const auto& sw = ite->second;
        const auto& group = sw.group;

        if (!group) {
          used.insert(swname);

          cattleshed::SingleSwitch* single =
              info->add_switches()->mutable_single();
          single->set_name(sw.name);
          single->set_display_name(sw.display_name);
          single->set_display_flags(
              sw.display_flags ? *sw.display_flags
                               : boost::algorithm::join(sw.flags, " "));
          single->set_default_value(compiler.initial_checked.count(sw.name) !=
                                    0);
        } else {
          std::unordered_set<std::string> groups;
          for (const auto& swname : compiler.switches) {
            const auto& sw = config.switches.at(swname);
            if (sw.group && *sw.group == *group) {
              groups.insert(sw.name);
            }
          }

          cattleshed::SelectSwitch* sel =
              info->add_switches()->mutable_select();
          std::string def = swname;
          for (const auto& swname : compiler.switches) {
            const auto& sw = config.switches.at(swname);
            if (groups.count(swname) == 0) {
              continue;
            }

            cattleshed::SelectSwitchOption* opt = sel->add_options();
            opt->set_name(sw.name);
            opt->set_display_name(sw.display_name);
            opt->set_display_flags(sw.display_flags
                                       ? *sw.display_flags
                                       : boost::algorithm::join(sw.flags, " "));

            if (compiler.initial_checked.count(sw.name) != 0) {
              def = swname;
            }
          }
          sel->set_name(*group);
          sel->set_default_value(def);

          used.insert(groups.begin(), groups.end());
        }
      }

      info->set_name(compiler.name);
      info->set_version(pair.second);
      info->set_language(compiler.language);
      info->set_display_name(compiler.display_name);
      info->set_display_compile_command(compiler.display_compile_command);
      info->set_compiler_option_raw(compiler.compiler_option_raw);
      info->set_runtime_option_raw(compiler.runtime_option_raw);
      for (const std::string& tmpl : compiler.templates) {
        info->add_templates(tmpl);
      }
    }
    for (const auto& pair : config.templates) {
      const wandbox::template_trait& t = pair.second;
      cattleshed::Template* tmpl = resp.add_templates();
      tmpl->set_name(t.name);
      tmpl->set_default_source(t.code);
      // TODO(melpon): sources を設定する（template_traits 側を直さないといけない）
      if (t.stdin) {
        tmpl->set_stdin(*t.stdin);
      }
      if (t.options) {
        tmpl->set_compiler_options(*t.options);
      }
      if (t.compiler_option_raw) {
        tmpl->set_compiler_option_raw(*t.compiler_option_raw);
      }
      if (t.runtime_option_raw) {
        tmpl->set_runtime_option_raw(*t.runtime_option_raw);
      }
    }

    return resp;
  }

 private:
  std::shared_ptr<boost::asio::io_context> ioc_;
  std::shared_ptr<boost::asio::signal_set> sigs_;
  const wandbox::server_config* config_;
  cattleshed::Cattleshed::AsyncService* service_;
  std::shared_ptr<VersionRunner> version_runner_;
};

class RunJobHandler
    : public ggrpc::ServerReaderWriterHandler<cattleshed::RunJobResponse,
                                              cattleshed::RunJobRequest> {
 public:
  RunJobHandler(cattleshed::Cattleshed::AsyncService* service,
                std::shared_ptr<boost::asio::io_context> ioc,
                std::shared_ptr<boost::asio::signal_set> sigs,
                const wandbox::server_config* config)
      : ioc_(ioc), sigs_(sigs), service_(service), config_(config) {}
  ~RunJobHandler() { SPDLOG_TRACE("[0x{}] deleted", (void*)this); }

 public:
  // Success を呼ばずに return した場合、必ず Finish を呼ぶガード
  struct FinishGuard {
    RunJobHandler* p;
    FinishGuard(RunJobHandler* p) : p(p) {}
    ~FinishGuard() {
      if (p) {
        p->Context()->Finish(grpc::Status::CANCELLED);
      }
    }
    void Success() { p = nullptr; }
  };

  void Request(
      grpc::ServerContext* context,
      grpc::ServerAsyncReaderWriter<cattleshed::RunJobResponse,
                                    cattleshed::RunJobRequest>* streamer,
      grpc::ServerCompletionQueue* cq, void* tag) override {
    service_->RequestRunJob(context, streamer, cq, cq, tag);
  }
  void OnAccept() override { SPDLOG_INFO("RunJobRequest::OnAccept"); }
  void OnRead(cattleshed::RunJobRequest req) override {
    SPDLOG_INFO("[0x{}] received RunJobRequest {}", (void*)this,
                req.DebugString());

    FinishGuard guard(this);

    // リクエストの種類が kStart じゃない
    if (req.data_case() != cattleshed::RunJobRequest::kStart) {
      SPDLOG_WARN("[0x{}] unknown enum value {}", (void*)this,
                  (int)req.data_case());
      return;
    }
    // 既に開始済み
    if (started_) {
      SPDLOG_WARN("[0x{}] already started", (void*)this);
      return;
    }

    req_start_ = req.start();

    const auto it = config_->compilers.get<1>().find(req_start_.compiler());
    // 設定が見つからない
    if (it == config_->compilers.get<1>().end()) {
      SPDLOG_ERROR("[0x{}] selected compiler '{}' is not configured",
                   (void*)this, req_start_.compiler());
      return;
    }

    // 実行開始
    started_ = true;

    // まずソースをファイルに書き込む
    // ここは sandbox の外なのですごく気をつける必要がある
    program_writer_.reset(
        new ProgramWriter(ioc_, *config_, *it, req_start_, sigs_));
    program_writer_->AsyncWriteProgram(std::bind(&RunJobHandler::OnWriteProgram,
                                                 this, std::placeholders::_1,
                                                 std::placeholders::_2));
    guard.Success();
  }

  void OnWriteProgram(const boost::system::error_code& ec,
                      std::shared_ptr<DIR> workdir) {
    FinishGuard guard(this);

    if (ec) {
      SPDLOG_ERROR("[0x{}] failed to write program: {}", (void*)this,
                   ec.message());
      return;
    }

    // ソースの書き込みが終わったらサンドボックス上でコンパイラとかを実行する
    program_writer_.reset();

    const wandbox::compiler_trait& target_compiler =
        *config_->compilers.get<1>().find(req_start_.compiler());
    auto send = [context = Context()](const cattleshed::RunJobResponse& resp) {
      context->Write(resp);
    };
    program_runner_.reset(new ProgramRunner(ioc_, *config_, req_start_, sigs_,
                                            workdir, target_compiler, send));
    program_runner_->AsyncRun(std::bind(&RunJobHandler::OnRun, this));
    guard.Success();
  }
  void OnRun() {
    auto context = Context();
    if (context) {
      context->Finish(grpc::Status::OK);
    }
  }

  void OnReadDoneOrError() {}

 private:
  class ProgramWriter {
   public:
    void AsyncWriteProgram(std::function<void(const boost::system::error_code&,
                                              std::shared_ptr<DIR>)>
                               cb) {
      cb_ = std::move(cb);

      std::string date;
      std::string time;
      {
        char s[64];
        time_t t = ::time(0);
        struct tm l;
        ::localtime_r(&t, &l);

        ::strftime(s, sizeof(s), "%Y%m%d", &l);
        date = s;
        ::strftime(s, sizeof(s), "%H%M%S", &l);
        time = s;
      }

      std::string unique_name;
      std::shared_ptr<DIR> workdir;
      while (unique_name.empty() || !workdir) {
        try {
          unique_name = wandbox::mkdtemp("wandbox_" + time + "_XXXXXX");
          workdir = wandbox::opendir(unique_name);
        } catch (std::system_error& e) {
          if (e.code().value() != ENOTDIR) {
            cb_(boost::system::error_code(e.code().value(),
                                          boost::system::generic_category()),
                nullptr);
            return;
          }
        }
      }

      std::string logdirnamebase = config_->system.storedir + "/" + date;
      std::string logdirname = logdirnamebase + "/" + unique_name;
      SPDLOG_INFO("[0x{}] create log directory '{}'", (void*)this, logdirname);
      const auto logdirbase =
          wandbox::mkdir_p_open_at(nullptr, logdirnamebase, 0700);
      const auto logdir =
          wandbox::mkdir_p_open_at(logdirbase, unique_name, 0700);
      if (!logdir) {
        SPDLOG_ERROR("[0x{}] failed to create log directory '{}'", (void*)this,
                     logdirname);
        cb_(boost::system::error_code(errno, boost::system::generic_category()),
            nullptr);
        return;
      }

      SPDLOG_INFO("[0x{}] using temporary name '{}'", (void*)this, unique_name);
      const auto savedir = wandbox::mkdir_p_open_at(workdir, "store", 0700);
      if (!savedir) {
        SPDLOG_ERROR("[0x{}] failed to create working directory '{}'",
                     (void*)this, unique_name);
        cb_(boost::system::error_code(errno, boost::system::generic_category()),
            nullptr);
        return;
      }

      std::unordered_multimap<std::string, std::shared_ptr<DIR>> dirs;
      dirs.emplace(std::string(), savedir);
      dirs.emplace(std::string(), logdir);

      {
        const SourceFile filebase(target_compiler_.output_file,
                                  req_->default_source(), nullptr);
        sources_.emplace_back(filebase, savedir);
        sources_.emplace_back(filebase, logdir);
      }
      for (int i = 0; i < req_->sources_size(); i++) {
        const cattleshed::Source& x = req_->sources(i);
        SPDLOG_INFO("[0x{}] registering file '{}'", (void*)this, x.file_name());
        if (x.file_name().empty()) {
          continue;
        }

        auto tree = wandbox::split_path_tree(x.file_name());
        if (tree.empty()) {
          continue;
        }
        tree.pop_back();

        for (size_t n = 0; n < tree.size(); ++n) {
          if (dirs.find(tree[n]) != dirs.end()) {
            continue;
          }

          SPDLOG_INFO("[0x{}] create source subdirectory '{}'", (void*)this,
                      tree[n]);
          const auto parents = dirs.equal_range(n == 0 ? "" : tree[n - 1]) |
                               boost::adaptors::map_values;
          const auto dirname =
              n == 0 ? tree[n] : tree[n].substr(tree[n - 1].length() + 1);
          for (auto&& p : std::vector<std::shared_ptr<DIR>>(parents.begin(),
                                                            parents.end())) {
            wandbox::mkdirat(p, dirname, 0700);
            dirs.emplace(tree[n], wandbox::opendirat(p, dirname));
          }
        }

        const auto filename =
            x.file_name().substr(x.file_name().find_last_of('/') + 1);
        const SourceFile filebase(filename, x.source(), nullptr);
        for (auto& p : dirs.equal_range(tree.empty() ? "" : tree.back()) |
                           boost::adaptors::map_values) {
          sources_.emplace_back(filebase, p);
        }
      }

      workdir_ = workdir;
      logdir_ = logdirbase;
      loginfoname_ = unique_name + ".json";
      {
        google::protobuf::util::JsonPrintOptions opt;
        opt.add_whitespace = true;
        opt.always_print_primitive_fields = true;
        // ソースの情報を以外を JSON 化する
        auto req = *req_;
        req.clear_default_source();
        req.clear_sources();
        google::protobuf::util::MessageToJsonString(req, &loginfocontent_, opt);
      }

      // まずソース以外の情報を書き込む
      DoWriteInfo();
    }

    void DoWriteInfo() {
      ::memset(&aiocb_, 0, sizeof(aiocb_));
      aiocb_.aio_fildes = ::openat(
          ::dirfd(logdir_.get()), ("./" + loginfoname_).c_str(),
          O_WRONLY | O_CLOEXEC | O_CREAT | O_TRUNC | O_EXCL | O_NOATIME, 0600);
      if (aiocb_.aio_fildes < 0) {
        if (errno == EAGAIN || errno == EMFILE || errno == EWOULDBLOCK) {
          SPDLOG_ERROR("[0x{}] open failed '{}'", (void*)this, loginfoname_);
          Complete(boost::system::error_code(
              errno, boost::system::generic_category()));
          return;
        } else {
          SPDLOG_ERROR("[0x{}] open failed '{}'", (void*)this, loginfoname_);
          Complete(boost::system::error_code(
              errno, boost::system::generic_category()));
          return;
        }
      }

      SPDLOG_DEBUG("[0x{}] write program '{}'", (void*)this, loginfoname_);
      SPDLOG_DEBUG("[0x{}] write program contents: {}", (void*)this,
                   loginfocontent_);
      aiocb_.aio_buf = &loginfocontent_[0];
      aiocb_.aio_nbytes = loginfocontent_.size();
      aiocb_.aio_sigevent.sigev_notify = SIGEV_SIGNAL;
      aiocb_.aio_sigevent.sigev_signo = SIGHUP;
      ::aio_write(&aiocb_);
      // 書き込みを待つ
      sigs_->async_wait(std::bind(&ProgramWriter::OnWriteInfo, this,
                                  std::placeholders::_1,
                                  std::placeholders::_2));
    }

    void OnWriteInfo(const boost::system::error_code& ec, int signum) {
      if (ec) {
        Complete(ec);
        return;
      }
      int error = ::aio_error(&aiocb_);
      // まだ終わってなかったので再度待ち状態へ
      if (error == EINPROGRESS) {
        sigs_->async_wait(std::bind(&ProgramWriter::OnWriteInfo, this,
                                    std::placeholders::_1,
                                    std::placeholders::_2));
        return;
      }

      ::close(aiocb_.aio_fildes);
      aiocb_.aio_fildes = -1;

      if (error != 0) {
        SPDLOG_ERROR("[0x{}] write failed '{}' error={}", (void*)this,
                     loginfoname_, error);
        Complete(boost::system::error_code(error,
                                           boost::system::generic_category()));
        return;
      }

      SPDLOG_INFO("[0x{}] write success '{}'", (void*)this, loginfoname_);

      // プログラムのソース情報を書き込む
      DoWriteProgram();
    }

    void DoWriteProgram() {
      SPDLOG_DEBUG("[0x{}] remain source: {}", (void*)this, sources_.size());

      // 全部のファイルを書き込み終わったら終了
      if (sources_.empty()) {
        Complete(boost::system::error_code());
        return;
      }

      const auto& source = sources_.front();

      ::memset(&aiocb_, 0, sizeof(aiocb_));
      aiocb_.aio_fildes = ::openat(
          ::dirfd(source.dir.get()), ("./" + source.name).c_str(),
          O_WRONLY | O_CLOEXEC | O_CREAT | O_TRUNC | O_EXCL | O_NOATIME, 0600);
      if (aiocb_.aio_fildes < 0) {
        if (errno == EAGAIN || errno == EMFILE || errno == EWOULDBLOCK) {
          // TODO(melpon): 一定時間後にリトライする
          // 今はエラーにしておく
          SPDLOG_ERROR("[0x{}] open failed '{}'", (void*)this, source.name);
          Complete(boost::system::error_code(
              errno, boost::system::generic_category()));
          return;
        } else {
          SPDLOG_ERROR("[0x{}] open failed '{}'", (void*)this, source.name);
          Complete(boost::system::error_code(
              errno, boost::system::generic_category()));
          return;
        }
      }
      SPDLOG_DEBUG("[0x{}] write program '{}'", (void*)this, source.name);
      SPDLOG_DEBUG("[0x{}] write program contents: {}", (void*)this,
                   std::string(source.buf(), source.buf() + source.len));
      aiocb_.aio_buf = source.buf();
      aiocb_.aio_nbytes = source.len;
      aiocb_.aio_sigevent.sigev_notify = SIGEV_SIGNAL;
      aiocb_.aio_sigevent.sigev_signo = SIGHUP;
      ::aio_write(&aiocb_);
      // 書き込みを待つ
      sigs_->async_wait(std::bind(&ProgramWriter::OnWriteProgram, this,
                                  std::placeholders::_1,
                                  std::placeholders::_2));
    }

    void OnWriteProgram(const boost::system::error_code& ec, int signum) {
      if (ec) {
        Complete(ec);
        return;
      }
      int error = ::aio_error(&aiocb_);
      // まだ終わってなかったので再度待ち状態へ
      if (error == EINPROGRESS) {
        sigs_->async_wait(std::bind(&ProgramWriter::OnWriteProgram, this,
                                    std::placeholders::_1,
                                    std::placeholders::_2));
        return;
      }

      const auto& source = sources_.front();

      ::close(aiocb_.aio_fildes);
      aiocb_.aio_fildes = -1;

      if (error != 0) {
        SPDLOG_ERROR("[0x{}] write failed '{}' error={}", (void*)this,
                     source.name, error);
        Complete(boost::system::error_code(error,
                                           boost::system::generic_category()));
        return;
      }

      SPDLOG_INFO("[0x{}] write success '{}'", (void*)this, source.name);
      sources_.pop_front();
      // 次のファイルを書き込む
      DoWriteProgram();
    }

    ProgramWriter(std::shared_ptr<boost::asio::io_context> ioc,
                  const wandbox::server_config& config,
                  const wandbox::compiler_trait& target_compiler,
                  const cattleshed::RunJobRequest::Start& req,
                  std::shared_ptr<boost::asio::signal_set> sigs)
        : ioc_(ioc),
          config_(&config),
          target_compiler_(target_compiler),
          req_(&req),
          sigs_(sigs) {
      ::memset(&aiocb_, 0, sizeof(aiocb_));
      aiocb_.aio_fildes = -1;
    }
    ~ProgramWriter() noexcept {
      if (aiocb_.aio_fildes != -1) {
        ::close(aiocb_.aio_fildes);
      }
    }

   private:
    void Complete(boost::system::error_code ec) {
      boost::asio::post(ioc_->get_executor(),
                        [ec, cb = std::move(cb_),
                         workdir = std::move(workdir_)]() { cb(ec, workdir); });
    }

    std::shared_ptr<DIR> logdir_;
    std::string loginfoname_;
    std::string loginfocontent_;

    std::shared_ptr<boost::asio::io_context> ioc_;
    std::shared_ptr<boost::asio::signal_set> sigs_;
    struct aiocb aiocb_;
    wandbox::compiler_trait target_compiler_;
    const cattleshed::RunJobRequest::Start* req_;
    std::function<void(const boost::system::error_code& error,
                       std::shared_ptr<DIR>)>
        cb_;
    const wandbox::server_config* config_;
    std::shared_ptr<DIR> workdir_;

    struct SourceFile {
      std::string name;
      std::shared_ptr<char> source_shared;
      std::size_t len;
      std::shared_ptr<DIR> dir;
      SourceFile() = default;
      SourceFile(std::string filename, const std::string& source,
                 std::shared_ptr<DIR> dir)
          : name(std::move(filename)),
            len(source.length()),
            dir(std::move(dir)) {
        source_shared.reset(new char[len], [](char* p) { delete[] p; });
        memcpy(source_shared.get(), source.c_str(), len);
      }
      SourceFile(const SourceFile& other, std::shared_ptr<DIR> dir)
          : name(other.name),
            source_shared(other.source_shared),
            len(other.len),
            dir(dir) {}
      SourceFile(const SourceFile&) = default;
      SourceFile& operator=(const SourceFile&) = default;
      SourceFile(SourceFile&&) = default;
      SourceFile& operator=(SourceFile&&) = default;
      char* buf() const { return source_shared.get(); }
    };
    std::deque<SourceFile> sources_;
  };

  struct ProgramRunner {
    struct CommandType {
      std::vector<std::string> arguments;
      std::string stdin;
      cattleshed::RunJobResponse::Type stdout_type;
      cattleshed::RunJobResponse::Type stderr_type;
      int soft_kill_wait;
    };

    struct PipeForwarderBase : boost::noncopyable {
      virtual void Close() noexcept = 0;
      virtual bool Closed() const noexcept = 0;
      virtual void AsyncForward(std::function<void()>) noexcept = 0;
    };

    struct StatusForwarder : PipeForwarderBase {
      StatusForwarder(std::shared_ptr<boost::asio::io_context> ioc,
                      std::shared_ptr<boost::asio::signal_set> sigs,
                      wandbox::unique_child_pid pid)
          : ioc_(ioc), sigs_(std::move(sigs)), pid_(std::move(pid)) {}
      void Close() noexcept override {}
      bool Closed() const noexcept override { return pid_.finished(); }
      void AsyncForward(std::function<void()> handler) noexcept override {
        sigs_->async_wait(std::bind(&StatusForwarder::OnWait, this, handler));
      }
      int GetStatus() noexcept { return pid_.wait_nonblock(); }
      void Kill(int signo) noexcept {
        if (!pid_.finished()) {
          int n = ::kill(pid_.get(), signo);
          if (n == 0) {
            SPDLOG_INFO("kill sent: signo={}", signo);
          } else {
            SPDLOG_ERROR("kill failed: signo={}, errno={}", signo, errno);
          }
        }
      }
      void OnWait(std::function<void()> handler) {
        pid_.wait_nonblock();
        if (not pid_.finished()) {
          // プロセスが終わってなかったので待ち直し
          AsyncForward(handler);
          return;
        }

        // プロセスが終わってたのでハンドラを読んで終了
        handler();
      }

     private:
      std::shared_ptr<boost::asio::io_context> ioc_;
      std::shared_ptr<boost::asio::signal_set> sigs_;
      wandbox::unique_child_pid pid_;
    };

    struct WriteLimitCounter {
      explicit WriteLimitCounter(size_t soft_limit, size_t hard_limit)
          : soft_limit_(soft_limit), hard_limit_(hard_limit), current_(0) {}
      void SetProcess(std::shared_ptr<StatusForwarder> proc) {
        proc_ = std::move(proc);
      }
      void Add(size_t len) {
        if (std::numeric_limits<size_t>::max() - len < current_) {
          current_ = std::numeric_limits<size_t>::max();
        } else {
          current_ += len;
        }
        if (auto p = proc_.lock()) {
          if (hard_limit_ < current_) {
            SPDLOG_WARN("output hard limit. send SIGKILL");
            p->Kill(SIGKILL);
          } else if (soft_limit_ < current_) {
            SPDLOG_WARN("output soft limit. send SIGXFSZ");
            p->Kill(SIGXFSZ);
          }
        }
      }

     private:
      size_t soft_limit_;
      size_t hard_limit_;
      size_t current_;
      std::weak_ptr<StatusForwarder> proc_;
    };

    struct InputForwarder : PipeForwarderBase {
      InputForwarder(std::shared_ptr<boost::asio::io_context> ioc,
                     wandbox::unique_fd&& fd, std::string input)
          : ioc_(ioc), pipe_(*ioc), input_(std::move(input)) {
        pipe_.assign(fd.get());
        fd.release();
      }
      void Close() noexcept override { pipe_.close(); }
      bool Closed() const noexcept override { return !pipe_.is_open(); }
      void AsyncForward(std::function<void()> handler) noexcept override {
        boost::asio::async_write(pipe_, boost::asio::buffer(input_),
                                 std::bind(&InputForwarder::OnWrite, this,
                                           handler, std::placeholders::_1));
      }
      void OnWrite(std::function<void()> handler,
                   boost::system::error_code ec) {
        pipe_.close();
        handler();
      }

     private:
      std::shared_ptr<boost::asio::io_context> ioc_;
      boost::asio::posix::stream_descriptor pipe_;
      std::string input_;
    };

    struct OutputForwarder : PipeForwarderBase {
      OutputForwarder(
          std::shared_ptr<boost::asio::io_context> ioc, wandbox::unique_fd fd,
          cattleshed::RunJobResponse::Type command_type,
          std::shared_ptr<WriteLimitCounter> limit,
          std::function<void(const cattleshed::RunJobResponse&)> send)
          : ioc_(ioc),
            pipe_(*ioc),
            command_type_(command_type),
            limit_(std::move(limit)),
            send_(std::move(send)) {
        pipe_.assign(fd.get());
        fd.release();
      }
      void Close() noexcept override { pipe_.close(); }
      bool Closed() const noexcept override { return !pipe_.is_open(); }
      void AsyncForward(std::function<void()> handler) noexcept override {
        handler_ = std::move(handler);
        buf_.resize(BUFSIZ);
        pipe_.async_read_some(
            boost::asio::buffer(buf_),
            std::bind(&OutputForwarder::OnRead, this, std::placeholders::_1,
                      std::placeholders::_2));
      }
      void OnRead(boost::system::error_code ec, size_t len) {
        if (ec) {
          pipe_.close();
          if (handler_) {
            boost::asio::post(ioc_->get_executor(), std::move(handler_));
            handler_ = {};
          }
          return;
        }
        cattleshed::RunJobResponse resp;
        resp.set_type(command_type_);
        resp.set_data(std::string(buf_.begin(), buf_.begin() + len));
        send_(resp);
        if (auto l = limit_.lock()) {
          l->Add(len);
        }

        // 再度読む
        pipe_.async_read_some(
            boost::asio::buffer(buf_),
            std::bind(&OutputForwarder::OnRead, this, std::placeholders::_1,
                      std::placeholders::_2));
      }

     private:
      std::shared_ptr<boost::asio::io_context> ioc_;
      boost::asio::posix::stream_descriptor pipe_;
      cattleshed::RunJobResponse::Type command_type_;
      std::vector<char> buf_;
      std::function<void()> handler_;
      std::weak_ptr<WriteLimitCounter> limit_;
      std::function<void(const cattleshed::RunJobResponse&)> send_;
    };

    ProgramRunner(std::shared_ptr<boost::asio::io_context> ioc,
                  const wandbox::server_config& config,
                  const cattleshed::RunJobRequest::Start& req,
                  std::shared_ptr<boost::asio::signal_set> sigs,
                  std::shared_ptr<DIR> workdir,
                  const wandbox::compiler_trait& target_compiler,
                  std::function<void(const cattleshed::RunJobResponse&)> send)
        : ioc_(ioc),
          config_(&config),
          req_(&req),
          sigs_(sigs),
          workdir_(std::move(workdir)),
          target_compiler_(target_compiler),
          send_(std::move(send)),
          kill_timer_(*ioc) {
      limitter_ = std::make_shared<WriteLimitCounter>(jail().output_limit_warn,
                                                      jail().output_limit_kill);
    }

    void AsyncRun(std::function<void()> cb) {
      SPDLOG_TRACE("running program with '{}'", target_compiler_.name);

      cb_ = std::move(cb);

      // コンパイル時と実行時のコマンドを作る
      {
        namespace qi = boost::spirit::qi;

        auto ccargs = target_compiler_.compile_command;
        auto progargs = target_compiler_.run_command;

        std::unordered_set<std::string> selected_switches;
        qi::parse(
            req_->compiler_options().begin(), req_->compiler_options().end(),
            qi::as_string[+(qi::char_ - ',' - '\n')] % ',', selected_switches);

        for (const auto& sw : selected_switches) {
          wandbox::switch_trait t;
          {
            const auto it = target_compiler_.local_switches.get<1>().find(sw);
            if (it == target_compiler_.local_switches.get<1>().end()) {
              const auto it2 = config_->switches.find(sw);
              if (it2 == config_->switches.end()) {
                SPDLOG_WARN("[0x{}] not found option '{}'", (void*)this, sw);
                continue;
              }
              t = it2->second;
            } else {
              t = *it;
            }
          }

          std::stringstream ss;
          for (auto&& f : t.flags) {
            ss << ' ' << f;
          }
          SPDLOG_INFO("[0x{}] using option {}:", (void*)this, ss.str());

          const auto f = [&t](std::vector<std::string>& args) {
            if (t.insert_position == 0) {
              args.insert(args.end(), t.flags.begin(), t.flags.end());
            } else {
              args.insert(args.begin() + t.insert_position, t.flags.begin(),
                          t.flags.end());
            }
          };
          f(t.runtime ? progargs : ccargs);
        }

        {
          const auto f = [](const std::string& rawopts,
                            std::vector<std::string>& args) {
            std::string input = rawopts;
            std::vector<std::string> s;
            boost::algorithm::replace_all(input, "\r\n", "\n");
            boost::algorithm::split(s, input, boost::is_any_of("\r\n"));
            if (not s.empty() && s.back().empty()) {
              s.pop_back();
            }
            args.insert(args.end(), s.begin(), s.end());
          };
          f(req_->compiler_option_raw(), ccargs);
          f(req_->runtime_option_raw(), progargs);
        }

        ccargs.insert(ccargs.begin(), jail().jail_command.begin(),
                      jail().jail_command.end());
        progargs.insert(progargs.begin(), jail().jail_command.begin(),
                        jail().jail_command.end());
        commands_ = {
            {std::move(ccargs), "", cattleshed::RunJobResponse::COMPILER_STDOUT,
             cattleshed::RunJobResponse::COMPILER_STDERR,
             jail().compile_time_limit},
            {std::move(progargs), req_->stdin(),
             cattleshed::RunJobResponse::STDOUT,
             cattleshed::RunJobResponse::STDERR, jail().program_duration}};
      }

      // 開始
      cattleshed::RunJobResponse resp;
      resp.set_type(cattleshed::RunJobResponse::CONTROL);
      resp.set_data("Start");
      send_(resp);

      DoRun();
    }

   private:
    void DoRun() {
      if (commands_.empty()) {
        // 全ての実行が終わったので終了
        Completed();
        return;
      }

      current_ = std::move(commands_.front());
      commands_.pop_front();

      std::stringstream ss;
      for (auto&& s : current_.arguments) {
        ss << s << ' ';
      }
      SPDLOG_INFO("[0x{}] exec {}", (void*)this, ss.str());

      {
        auto c = wandbox::piped_spawn(workdir_, current_.arguments);

        pipes_ = {
            std::make_shared<InputForwarder>(ioc_, std::move(c.fd_stdin),
                                             current_.stdin),
            std::make_shared<OutputForwarder>(ioc_, std::move(c.fd_stdout),
                                              current_.stdout_type, limitter_,
                                              send_),
            std::make_shared<OutputForwarder>(ioc_, std::move(c.fd_stderr),
                                              current_.stderr_type, limitter_,
                                              send_),
            std::make_shared<StatusForwarder>(ioc_, sigs_, std::move(c.pid)),
        };
        limitter_->SetProcess(
            std::static_pointer_cast<StatusForwarder>(pipes_[3]));
      }

      pipes_[0]->AsyncForward(std::bind(&ProgramRunner::OnForward, this));
      pipes_[1]->AsyncForward(std::bind(&ProgramRunner::OnForward, this));
      pipes_[2]->AsyncForward(std::bind(&ProgramRunner::OnForward, this));
      pipes_[3]->AsyncForward(std::bind(&ProgramRunner::OnForward, this));

      kill_timer_.expires_from_now(
          boost::posix_time::seconds(current_.soft_kill_wait));
      kill_timer_.async_wait(
          std::bind(&ProgramRunner::OnTimeout, this, std::placeholders::_1));
    }

    void OnForward() {
      //SPDLOG_TRACE("OnForward: pipes[0] is {}",
      //             pipes_[0]->Closed() ? "closed" : "opened");
      //SPDLOG_TRACE("OnForward: pipes[1] is {}",
      //             pipes_[1]->Closed() ? "closed" : "opened");
      //SPDLOG_TRACE("OnForward: pipes[2] is {}",
      //             pipes_[2]->Closed() ? "closed" : "opened");
      //SPDLOG_TRACE("OnForward: pipes[3] is {}",
      //             pipes_[3]->Closed() ? "closed" : "opened");

      if (not std::all_of(pipes_.begin(), pipes_.end(),
                          [](std::shared_ptr<PipeForwarderBase> p) {
                            return p->Closed();
                          })) {
        // まだ全部の Forward が終わってないので更に待つ
        return;
      }

      // 実行完了した
      kill_timer_.cancel();
      laststatus_ =
          std::static_pointer_cast<StatusForwarder>(pipes_[3])->GetStatus();
      // 実行に失敗したのでここで終了処理
      if (!WIFEXITED(laststatus_) || (WEXITSTATUS(laststatus_) != 0)) {
        Completed();
        return;
      }

      // 実行に成功したので次のコマンド実行
      DoRun();
    }

    void OnTimeout(const boost::system::error_code& ec) {
      if (ec) {
        // タイマーがキャンセルされた（＝実行が成功した）
        return;
      }

      SPDLOG_INFO("[0x{}] exec timeout, send SIGXCPU", (void*)this);

      // タイムアウトしたので SIGXCPU する
      std::static_pointer_cast<StatusForwarder>(pipes_[3])->Kill(SIGXCPU);
      // さらに一定時間経過したら SIGKILL する
      kill_timer_.expires_from_now(
          boost::posix_time::seconds(jail().kill_wait));
      kill_timer_.async_wait(std::bind(&ProgramRunner::OnSignalTimeout, this,
                                       std::placeholders::_1));
    }

    void OnSignalTimeout(const boost::system::error_code& ec) {
      if (ec) {
        // タイマーがキャンセルされた（＝SIGXCPUでとりあえず実行が終わった）
        return;
      }

      SPDLOG_INFO("[0x{}] exec timeout, send SIGKILL", (void*)this);

      // SIGXCPU だとダメだったので SIGKILL
      std::static_pointer_cast<StatusForwarder>(pipes_[3])->Kill(SIGKILL);
    }

    void Completed() {
      if (WIFEXITED(laststatus_)) {
        cattleshed::RunJobResponse resp;
        resp.set_type(cattleshed::RunJobResponse::EXIT_CODE);
        resp.set_data(std::to_string(WEXITSTATUS(laststatus_)));
        send_(resp);
      }
      if (WIFSIGNALED(laststatus_)) {
        cattleshed::RunJobResponse resp;
        resp.set_type(cattleshed::RunJobResponse::SIGNAL);
        resp.set_data(::strsignal(WTERMSIG(laststatus_)));
        send_(resp);
      }

      cattleshed::RunJobResponse resp;
      resp.set_type(cattleshed::RunJobResponse::CONTROL);
      resp.set_data("Finish");
      send_(resp);

      cb_();

      SPDLOG_INFO("[0x{}] finished", (void*)this);
    }

    const wandbox::jail_config& jail() const {
      return config_->jails.at(target_compiler_.jail_name);
    }

    std::shared_ptr<boost::asio::io_context> ioc_;
    const wandbox::server_config* config_;
    const cattleshed::RunJobRequest::Start* req_;
    std::shared_ptr<DIR> workdir_;
    std::shared_ptr<boost::asio::signal_set> sigs_;
    wandbox::compiler_trait target_compiler_;
    std::function<void(const cattleshed::RunJobResponse&)> send_;

    std::function<void()> cb_;

    std::vector<std::shared_ptr<PipeForwarderBase>> pipes_;
    boost::asio::deadline_timer kill_timer_;
    std::deque<CommandType> commands_;
    CommandType current_;
    std::shared_ptr<WriteLimitCounter> limitter_;
    int laststatus_ = 0;
  };

 private:
  cattleshed::Cattleshed::AsyncService* service_;
  std::shared_ptr<boost::asio::io_context> ioc_;
  std::shared_ptr<boost::asio::signal_set> sigs_;
  const wandbox::server_config* config_;
  bool started_ = false;
  std::shared_ptr<ProgramWriter> program_writer_;
  std::shared_ptr<ProgramRunner> program_runner_;
  cattleshed::RunJobRequest::Start req_start_;
};

class CattleshedServer {
 public:
  CattleshedServer(std::shared_ptr<boost::asio::io_context> ioc,
                   wandbox::server_config config)
      : ioc_(ioc), config_(std::move(config)) {
    sigs_ = std::make_shared<boost::asio::signal_set>(*ioc_, SIGCHLD, SIGHUP);

    try {
      wandbox::mkdir(config_.system.basedir, 0700);
    } catch (std::system_error& e) {
      if (e.code().value() != EEXIST) {
        SPDLOG_ERROR(
            "failed to create basedir, check permission. error={} message=",
            e.code().value(), e.code().message());
        throw;
      }
    }
    auto basedir = wandbox::opendir(config_.system.basedir);
    wandbox::chdir(basedir);
  }

  void Start(std::string address, int threads) {
    grpc::ServerBuilder builder;
    builder.AddListeningPort(address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service_);

    SPDLOG_INFO("gRPC Server listening on {}", address);

    // ハンドラの登録
    server_.AddResponseWriterHandler<GetVersionHandler>(&service_, ioc_, sigs_,
                                                        &config_);
    server_.AddReaderWriterHandler<RunJobHandler>(&service_, ioc_, sigs_,
                                                  &config_);

    server_.Start(builder, threads);
  }
  void Wait() { server_.Wait(); }

 private:
  ggrpc::Server server_;
  cattleshed::Cattleshed::AsyncService service_;
  std::shared_ptr<boost::asio::io_context> ioc_;
  std::shared_ptr<boost::asio::signal_set> sigs_;
  wandbox::server_config config_;
};


#endif // CATTLESHED_SERVER_H_INCLUDED
