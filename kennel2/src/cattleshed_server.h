#ifndef CATTLESHED_SERVER_H_INCLUDED
#define CATTLESHED_SERVER_H_INCLUDED

#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <thread>
#include <tuple>

// gRPC
#include <grpcpp/alarm.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/support/async_stream.h>
#include <grpcpp/support/async_unary_call.h>

// spdlog
#include <spdlog/spdlog.h>

struct CattleshedServerHandler {
  virtual void Proceed(bool ok) = 0;
};

class GetVersionHandler {
  std::mutex mutex_;
  grpc::ServerCompletionQueue* cq_;

  grpc::ServerContext server_context_;
  grpc::ServerAsyncResponseWriter<cattleshed::GetVersionResponse>
      response_writer_;

  std::function<bool()> shutdown_requested_;

  cattleshed::GetVersionRequest request_;
  struct ResponseData {
    bool error;
    // finish == false
    cattleshed::GetVersionResponse response;
    // finish == true
    grpc::Status status;
  };
  ResponseData response_;

  struct AcceptorThunk : CattleshedServerHandler {
    GetVersionHandler* p;
    AcceptorThunk(GetVersionHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToAccept(ok); }
  };
  AcceptorThunk acceptor_thunk_;
  friend class AcceptorThunk;

  struct WriterThunk : CattleshedServerHandler {
    GetVersionHandler* p;
    WriterThunk(GetVersionHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToWrite(ok); }
  };
  WriterThunk writer_thunk_;
  friend class WriterThunk;

  struct NotifierThunk : CattleshedServerHandler {
    GetVersionHandler* p;
    NotifierThunk(GetVersionHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToNotify(ok); }
  };
  NotifierThunk notifier_thunk_;
  friend class NotifierThunk;

  // 状態マシン
  enum class WriteStatus { LISTENING, IDLE, FINISHING, FINISHED };
  WriteStatus write_status_ = WriteStatus::LISTENING;

  grpc::Alarm alarm_;
  void Notify() { alarm_.Set(cq_, gpr_time_0(GPR_TIMESPAN), &notifier_thunk_); }

  cattleshed::Cattleshed::AsyncService* service_;

 public:
  GetVersionHandler(std::function<bool()> shutdown_requested,
                    grpc::ServerCompletionQueue* cq,
                    cattleshed::Cattleshed::AsyncService* service)
      : shutdown_requested_(std::move(shutdown_requested)),
        cq_(cq),
        response_writer_(&server_context_),
        acceptor_thunk_(this),
        writer_thunk_(this),
        notifier_thunk_(this),
        service_(service) {
    Request(&server_context_, &request_, &response_writer_, cq_,
            &acceptor_thunk_);
  }

  typedef cattleshed::GetVersionResponse WriteType;
  typedef cattleshed::GetVersionRequest ReadType;

  void Finish(cattleshed::GetVersionResponse resp, grpc::Status status) {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      response_.error = false;
      response_.response = std::move(resp);
      response_.status = status;

      write_status_ = WriteStatus::FINISHING;
      Notify();
    }
  }

  void FinishWithError(grpc::Status status) {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      response_.error = true;
      response_.status = status;

      write_status_ = WriteStatus::FINISHING;
      Notify();
    }
  }

 private:
  void ProceedToAccept(bool ok) {
    std::unique_ptr<GetVersionHandler> p;
    std::unique_lock<std::mutex> lock(mutex_);

    SPDLOG_TRACE("ProceedToAccept: ok={}", ok);

    // サーバのシャットダウン要求が来てたら次の Accept 待ちをしない
    if (shutdown_requested_()) {
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    // Accept の失敗も次の Accept 待ちをしない
    if (!ok) {
      SPDLOG_ERROR("Accept failed");
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    // 次の要求に備える
    new GetVersionHandler(shutdown_requested_, cq_, service_);

    write_status_ = WriteStatus::IDLE;

    lock.unlock();
    try {
      OnAccept(std::move(request_));
      lock.lock();
    } catch (...) {
      lock.lock();
    }
  }

  void ProceedToWrite(bool ok) {
    std::unique_ptr<GetVersionHandler> p;
    std::unique_lock<std::mutex> lock(mutex_);

    SPDLOG_TRACE("ProceedToWrite: ok={}", ok);

    if (shutdown_requested_()) {
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    if (!ok) {
      SPDLOG_ERROR("write failed");
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    write_status_ = WriteStatus::FINISHED;
    p.reset(this);
  }

  void ProceedToNotify(bool ok) {
    std::unique_ptr<GetVersionHandler> p;
    std::lock_guard<std::mutex> guard(mutex_);

    SPDLOG_TRACE("ProceedToNotify: ok={}", ok);

    if (shutdown_requested_()) {
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    if (!ok) {
      SPDLOG_WARN("Alarm cancelled");
      write_status_ = WriteStatus::FINISHED;
      p.reset(this);
      return;
    }

    if (!response_.error) {
      response_writer_.Finish(std::move(response_.response), response_.status,
                              &writer_thunk_);
    } else {
      response_writer_.FinishWithError(response_.status, &writer_thunk_);
    }
    write_status_ = WriteStatus::FINISHING;
  }

private:
 void Request(grpc::ServerContext* context,
              cattleshed::GetVersionRequest* request,
              grpc::ServerAsyncResponseWriter<cattleshed::GetVersionResponse>*
                  response_writer,
              grpc::ServerCompletionQueue* cq, void* tag) {
   service_->RequestGetVersion(context, request, response_writer, cq, cq, tag);
  }
  void OnAccept(cattleshed::GetVersionRequest request) {
    cattleshed::GetVersionResponse resp;
    Finish(resp, grpc::Status::OK);
  }
};

class RunJobHandler {
  std::mutex mutex_;
  grpc::ServerCompletionQueue* cq_;

  grpc::ServerContext server_context_;
  grpc::ServerAsyncReaderWriter<cattleshed::RunJobResponse, cattleshed::RunJobRequest> streamer_;

  std::function<bool()> shutdown_requested_;

  cattleshed::RunJobRequest request_;
  struct ResponseData {
    bool finish;
    // finish == false
    cattleshed::RunJobResponse response;
    // finish == true
    grpc::Status status;
  };
  std::deque<ResponseData> response_queue_;

  struct AcceptorThunk : CattleshedServerHandler {
    RunJobHandler* p;
    AcceptorThunk(RunJobHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToAccept(ok); }
  };
  AcceptorThunk acceptor_thunk_;
  friend class AcceptorThunk;

  struct ReaderThunk : CattleshedServerHandler {
    RunJobHandler* p;
    ReaderThunk(RunJobHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToRead(ok); }
  };
  ReaderThunk reader_thunk_;
  friend class ReaderThunk;

  struct WriterThunk : CattleshedServerHandler {
    RunJobHandler* p;
    WriterThunk(RunJobHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToWrite(ok); }
  };
  WriterThunk writer_thunk_;
  friend class WriterThunk;

  struct NotifierThunk : CattleshedServerHandler {
    RunJobHandler* p;
    NotifierThunk(RunJobHandler* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToNotify(ok); }
  };
  NotifierThunk notifier_thunk_;
  friend class NotifierThunk;

  // 状態マシン
  enum class ReadStatus { LISTENING, READING, FINISHED };
  enum class WriteStatus { LISTENING, WRITING, IDLE, FINISHING, FINISHED };
  ReadStatus read_status_ = ReadStatus::LISTENING;
  WriteStatus write_status_ = WriteStatus::LISTENING;

  grpc::Alarm notify_alarm_;
  void Notify() {
    notify_alarm_.Set(cq_, gpr_time_0(GPR_TIMESPAN), &notifier_thunk_);
  }

  cattleshed::Cattleshed::AsyncService* service_;

 public:
  RunJobHandler(std::function<bool()> shutdown_requested,
                grpc::ServerCompletionQueue* cq,
                cattleshed::Cattleshed::AsyncService* service)
      : streamer_(&server_context_),
        shutdown_requested_(std::move(shutdown_requested)),
        cq_(cq),
        acceptor_thunk_(this),
        reader_thunk_(this),
        writer_thunk_(this),
        notifier_thunk_(this),
        service_(service) {
    Request(&server_context_, &streamer_, cq_, &acceptor_thunk_);
  }
  ~RunJobHandler() { SPDLOG_TRACE("[0x{}] deleted", (void*)this); }

  typedef cattleshed::RunJobResponse WriteType;
  typedef cattleshed::RunJobRequest ReadType;

  void Write(cattleshed::RunJobResponse resp) {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      ResponseData d;
      d.finish = false;
      d.response = std::move(resp);

      write_status_ = WriteStatus::WRITING;
      response_queue_.push_back(std::move(d));
      Notify();
    } else if (write_status_ == WriteStatus::WRITING) {
      ResponseData d;
      d.finish = false;
      d.response = std::move(resp);

      response_queue_.push_back(std::move(d));
    }
  }

  void Finish(grpc::Status status) {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      ResponseData d;
      d.finish = false;
      d.status = std::move(status);

      write_status_ = WriteStatus::FINISHING;
      response_queue_.push_back(std::move(d));
      Notify();
    } else if (write_status_ == WriteStatus::WRITING) {
      ResponseData d;
      d.finish = false;
      d.status = std::move(status);

      response_queue_.push_back(std::move(d));
    }
  }

 private:
  void FinishRead() {
    if (read_status_ == ReadStatus::LISTENING ||
        read_status_ == ReadStatus::READING) {
      server_context_.TryCancel();
    } else {
      read_status_ = ReadStatus::FINISHED;
    }
  }
  void FinishWrite() {
    if (write_status_ == WriteStatus::LISTENING ||
        write_status_ == WriteStatus::WRITING ||
        write_status_ == WriteStatus::FINISHING) {
      server_context_.TryCancel();
    } else {
      write_status_ = WriteStatus::FINISHED;
    }
  }

  bool Deletable() {
    return read_status_ == ReadStatus::FINISHED &&
           write_status_ == WriteStatus::FINISHED;
  }

  void ProceedToAccept(bool ok) {
    std::unique_ptr<RunJobHandler> p;
    std::unique_lock<std::mutex> lock(mutex_);

    SPDLOG_TRACE("[0x{}] ProceedToAccept: ok={}", (void*)this, ok);

    // サーバのシャットダウン要求が来てたら次の Accept 待ちをしない
    if (shutdown_requested_()) {
      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    // Accept の失敗も次の Accept 待ちをしない
    if (!ok) {
      SPDLOG_ERROR("Accept failed");
      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    // 次の要求に備える
    new RunJobHandler(shutdown_requested_, cq_, service_);

    write_status_ = WriteStatus::IDLE;

    lock.unlock();
    try {
      OnAccept();
      lock.lock();
    } catch (...) {
      lock.lock();
    }

    read_status_ = ReadStatus::READING;
    streamer_.Read(&request_, &reader_thunk_);
  }

  void ProceedToRead(bool ok) {
    std::unique_ptr<RunJobHandler> p;
    std::unique_lock<std::mutex> lock(mutex_);

    SPDLOG_TRACE("[0x{}] ProceedToRead: ok={}", (void*)this, ok);

    if (shutdown_requested_()) {
      read_status_ = ReadStatus::FINISHED;
      FinishWrite();
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      // 読み込みがすべて完了した（あるいは失敗した）
      // あとは書き込み処理が終わるのを待つだけ
      lock.unlock();
      try {
        OnReadDoneOrError();
      } catch (...) {
        lock.lock();
      }

      read_status_ = ReadStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    lock.unlock();
    try {
      OnRead(std::move(request_));
      lock.lock();
    } catch (...) {
      lock.lock();
    }

    // 次の読み込み
    streamer_.Read(&request_, &reader_thunk_);
  }

  void ProceedToWrite(bool ok) {
    std::unique_ptr<RunJobHandler> p;
    std::unique_lock<std::mutex> lock(mutex_);

    SPDLOG_TRACE("[0x{}] ProceedToWrite: ok={}", (void*)this, ok);

    if (shutdown_requested_()) {
      FinishRead();
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      SPDLOG_ERROR("write failed");
      FinishRead();
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
    } else if (write_status_ == WriteStatus::WRITING) {
      response_queue_.pop_front();
      if (response_queue_.empty()) {
        write_status_ = WriteStatus::IDLE;
      } else {
        auto& d = response_queue_.front();
        if (!d.finish) {
          // Response
          streamer_.Write(std::move(d.response), &writer_thunk_);
        } else {
          // Finish
          streamer_.Finish(d.status, &writer_thunk_);
          write_status_ = WriteStatus::FINISHING;
        }
      }
    } else if (write_status_ == WriteStatus::FINISHING) {
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
    }
  }

  void ProceedToNotify(bool ok) {
    std::unique_ptr<RunJobHandler> p;
    std::lock_guard<std::mutex> guard(mutex_);

    SPDLOG_TRACE("[0x{}] ProceedToNotify: ok={}", (void*)this, ok);

    if (shutdown_requested_()) {
      FinishRead();
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      SPDLOG_WARN("Alarm cancelled");
      if (read_status_ == ReadStatus::LISTENING ||
          read_status_ == ReadStatus::READING) {
        server_context_.TryCancel();
      } else {
        read_status_ = ReadStatus::FINISHED;
      }
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (response_queue_.empty()) {
      SPDLOG_WARN("response_queue_ is empty");
      FinishRead();
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    auto& d = response_queue_.front();
    if (!d.finish) {
      // Response
      streamer_.Write(std::move(d.response), &writer_thunk_);
    } else {
      // Finish
      streamer_.Finish(d.status, &writer_thunk_);
      write_status_ = WriteStatus::FINISHING;
    }
  }

 public:
  void Request(
      grpc::ServerContext* context,
      grpc::ServerAsyncReaderWriter<cattleshed::RunJobResponse,
                                    cattleshed::RunJobRequest>* streamer,
      grpc::ServerCompletionQueue* cq, void* tag) {
    service_->RequestRunJob(context, streamer, cq, cq, tag);
  }
  void OnAccept() {}
  void OnRead(cattleshed::RunJobRequest req) {
    SPDLOG_INFO("received RunJobRequest {}", req.DebugString());
    Finish(grpc::Status::OK);
  }
  void OnReadDoneOrError() {}
};

class CattleshedServer {
  std::vector<std::unique_ptr<grpc::ServerCompletionQueue>> cqs_;
  std::vector<std::unique_ptr<std::thread>> threads_;
  std::unique_ptr<grpc::Server> server_;
  std::mutex mutex_;

  std::atomic<bool> shutdown_{false};

  cattleshed::Cattleshed::AsyncService service_;

 public:
  ~CattleshedServer() { Shutdown(); }

  void Start(std::string address, int threads) {
    std::lock_guard<std::mutex> guard(mutex_);

    // 既に Start 済み
    if (threads_.size() != 0) {
      return;
    }
    // 既に Shutdown 済み
    if (shutdown_) {
      return;
    }

    grpc::ServerBuilder builder;
    builder.AddListeningPort(address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service_);

    std::vector<std::unique_ptr<grpc::ServerCompletionQueue>> cqs;
    for (int i = 0; i < threads; i++) {
      cqs.push_back(builder.AddCompletionQueue());
    }

    server_ = builder.BuildAndStart();
    cqs_ = std::move(cqs);

    SPDLOG_INFO("gRPC Server listening on {}", address);

    for (int i = 0; i < cqs_.size(); i++) {
      auto cq = cqs_[i].get();
      threads_.push_back(std::unique_ptr<std::thread>(
          new std::thread([this, cq] { this->HandleRpcs(cq); })));
    }
  }

  void Shutdown() {
    std::unique_lock<std::mutex> lock(mutex_);

    // Start してない
    if (threads_.size() == 0) {
      return;
    }
    if (shutdown_) {
      return;
    }
    shutdown_ = true;

    SPDLOG_INFO("Server Shutdown start");

    server_->Shutdown();
    // サーバをシャットダウンした後に completion queue を削除する必要がある
    for (auto& cq : cqs_) {
      cq->Shutdown();
    }

    SPDLOG_INFO("Server Shutdown waiting");

    for (auto& thread : threads_) {
      lock.unlock();
      try {
        thread->join();
        lock.lock();
      } catch (...) {
        lock.lock();
      }
    }

    SPDLOG_INFO("Server Shutdown completed");
  }

 private:
  void HandleRpcs(grpc::ServerCompletionQueue* cq) {
    auto shutdown_requested = [this]() -> bool { return shutdown_; };
    new GetVersionHandler(shutdown_requested, cq, &service_);
    new RunJobHandler(shutdown_requested, cq, &service_);

    void* got_tag = nullptr;
    bool ok = false;

    while (cq->Next(&got_tag, &ok)) {
      CattleshedServerHandler* call = static_cast<CattleshedServerHandler*>(got_tag);
      call->Proceed(ok);
    }
  }
};


#endif // CATTLESHED_SERVER_H_INCLUDED
