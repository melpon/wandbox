#ifndef CATTLESHED_CLIENT_H_INCLUDED
#define CATTLESHED_CLIENT_H_INCLUDED

#include <atomic>
#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <thread>

// grpc
#include <grpcpp/alarm.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/support/async_stream.h>
#include <grpcpp/support/async_unary_call.h>

// spdlog
#include <spdlog/spdlog.h>

#include "cattleshed.grpc.pb.h"
#include "cattleshed.pb.h"

enum GetVersionClientError {
  FINISH,
};

enum RunJobClientError {
  CONNECT,
  READ,
  WRITE,
};

struct CattleshedClientHandler {
  virtual void Proceed(bool ok) = 0;
};

class GetVersionClient {
  struct ReaderThunk : CattleshedClientHandler {
    GetVersionClient* p;
    ReaderThunk(GetVersionClient* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToRead(ok); }
  };
  ReaderThunk reader_thunk_;
  friend class ReaderThunk;

  grpc::ClientContext context_;
  std::unique_ptr<
      grpc::ClientAsyncResponseReader<cattleshed::GetVersionResponse>>
      reader_;

  cattleshed::GetVersionResponse response_;
  grpc::Status grpc_status_;

  bool shutdown_ = false;
  bool done_ = false;
  std::mutex mutex_;

  std::function<bool()> shutdown_requested_;
  grpc::CompletionQueue *cq_;

  typedef std::function<std::unique_ptr<
      grpc::ClientAsyncResponseReader<cattleshed::GetVersionResponse>>(
      grpc::ClientContext*, const cattleshed::GetVersionRequest&,
      grpc::CompletionQueue*)>
      request_t;
  request_t request_;
  typedef std::function<void(cattleshed::GetVersionResponse, grpc::Status)>
      on_response_t;
  on_response_t on_response_;
  typedef std::function<void(GetVersionClientError)> on_error_t;
  on_error_t on_error_;

  bool running_callback_ = false;

 public:
  GetVersionClient(grpc::CompletionQueue* cq,
                   std::function<bool()> shutdown_requested, request_t request)
      : reader_thunk_(this),
        cq_(cq),
        shutdown_requested_(std::move(shutdown_requested)),
        request_(std::move(request)) {}

  ~GetVersionClient() { SPDLOG_TRACE("[0x{}] deleted", (void*)this); }

  // コピー、ムーブ禁止
  GetVersionClient(const GetVersionClient&) = delete;
  GetVersionClient(GetVersionClient&&) = delete;
  GetVersionClient& operator=(const GetVersionClient&) = delete;
  GetVersionClient& operator=(GetVersionClient&&) = delete;

  void SetOnResponse(on_response_t on_response) {
    std::lock_guard<std::mutex> guard(mutex_);
    SPDLOG_TRACE("SetOnResponse start");
    if (shutdown_ || shutdown_requested_()) {
      SPDLOG_TRACE("SetOnResponse shutdown requested");
      return;
    }
    on_response_ = std::move(on_response);
    SPDLOG_TRACE("SetOnResponse end");
  }
  void SetOnError(on_error_t on_error) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_error_ = std::move(on_error_);
  }

  void Request(const cattleshed::GetVersionRequest& request) {
    std::lock_guard<std::mutex> guard(mutex_);
    reader_ = request_(&context_, request, cq_);
    reader_->Finish(&response_, &grpc_status_, &reader_thunk_);
  }

  void Shutdown() {
    std::unique_ptr<GetVersionClient> p;
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    shutdown_ = true;

    on_response_ = nullptr;
    on_error_ = nullptr;

    if (!done_) {
      // キャンセルされるまで待つ
      context_.TryCancel();
    }

    if (Deletable()) {
      p.reset(this);
    }
  }

 private:
  template <class F, class... Args>
  void RunCallback(std::unique_lock<std::mutex>& lock, F f, Args&&... args) {
    // 普通にコールバックするとデッドロックの可能性があるので
    // unlock してからコールバックする。
    // 再度ロックした時に状態が変わってる可能性があるので注意すること。
    if (f) {
      running_callback_ = true;
      lock.unlock();
      try {
        f(std::forward<Args>(args)...);
        // f に自身の shared_ptr がバインドされていて、コールバック中に SetOn〜 で nullptr が渡されてた場合、
        // RunCallback を抜ける時に Shutdown が呼ばれる可能性があり、ロック中に Shutdown するとデッドロックするので
        // アンロック中に f を無効にする必要がある。
        f = nullptr;
        lock.lock();
        running_callback_ = false;
      } catch (...) {
        lock.lock();
        running_callback_ = false;
      }
    }
  }

  template <class F, class... Args>
  void RunCallbackOnce(std::unique_lock<std::mutex>& lock, F& f,
                       Args&&... args) {
    // 呼び出した後に f が無効になるバージョン。
    // f に自身の shared_ptr がバインドされている場合、無効にした時点で
    // Shutdown が呼ばれる可能性があり、ロック中に Shutdown するとデッドロックするので
    // アンロック中に f を無効にする必要がある。
    if (f) {
      auto g = std::move(f);
      running_callback_ = true;
      lock.unlock();
      try {
        g(std::forward<Args>(args)...);
        g = nullptr;
        lock.lock();
        running_callback_ = false;
      } catch (...) {
        lock.lock();
        running_callback_ = false;
      }
    }
  }

  void ProceedToRead(bool ok) {
    std::unique_ptr<GetVersionClient> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("[0x{}] ProceedToRead: ok={}", (void*)this, ok);

    done_ = true;

    if (shutdown_ || shutdown_requested_()) {
      SPDLOG_INFO("shutdown GetVersionClient");
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      SPDLOG_ERROR("finishing error");
      RunCallbackOnce(lock, on_error_, GetVersionClientError::FINISH);
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    // 結果が取得できた
    RunCallbackOnce(lock, on_response_, std::move(response_),
                    std::move(grpc_status_));
    if (Deletable()) {
      p.reset(this);
    }
  }

  bool Deletable() {
    return (shutdown_ || shutdown_requested_()) && !running_callback_ && done_;
  }
};

class RunJobClient {
  struct ConnectorThunk : CattleshedClientHandler {
    RunJobClient* p;
    ConnectorThunk(RunJobClient* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToConnect(ok); }
  };
  ConnectorThunk connector_thunk_;
  friend class ConnectorThunk;

  struct ReaderThunk : CattleshedClientHandler {
    RunJobClient* p;
    ReaderThunk(RunJobClient* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToRead(ok); }
  };
  ReaderThunk reader_thunk_;
  friend class ReaderThunk;

  struct WriterThunk : CattleshedClientHandler {
    RunJobClient* p;
    WriterThunk(RunJobClient* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToWrite(ok); }
  };
  WriterThunk writer_thunk_;
  friend class WriterThunk;

  // ClientAsyncReaderWriter よりも ClientContext
  // の方が寿命が長くなるようにしないといけないので、 必ず streamer_ より上に
  // context_ を定義すること
  grpc::ClientContext context_;
  std::unique_ptr<grpc::ClientAsyncReaderWriter<cattleshed::RunJobRequest,
                                                cattleshed::RunJobResponse>>
      streamer_;

  enum class ReadStatus { CONNECTING, READING, FINISHING, FINISHED };
  ReadStatus read_status_ = ReadStatus::FINISHED;
  enum class WriteStatus { CONNECTING, WRITING, IDLE, FINISHING, FINISHED };
  WriteStatus write_status_ = WriteStatus::FINISHED;

  std::deque<std::shared_ptr<void>> request_queue_;
  cattleshed::RunJobResponse response_;

  grpc::Status grpc_status_;

  bool shutdown_ = false;
  std::mutex mutex_;

  std::function<bool()> shutdown_requested_;
  grpc::CompletionQueue* cq_;
  typedef std::function<std::unique_ptr<grpc::ClientAsyncReaderWriter<
      cattleshed::RunJobRequest, cattleshed::RunJobResponse>>(
      grpc::ClientContext*, grpc::CompletionQueue*, void*)>
      connect_t;
  connect_t connect_;
  typedef std::function<void()> on_connect_t;
  on_connect_t on_connect_;
  typedef std::function<void(cattleshed::RunJobResponse)> on_read_t;
  on_read_t on_read_;
  typedef std::function<void(grpc::Status)> on_read_done_t;
  on_read_done_t on_read_done_;
  typedef std::function<void(RunJobClientError)> on_error_t;
  on_error_t on_error_;
  typedef std::function<void()> on_writes_done_t;
  on_writes_done_t on_writes_done_;

  bool running_callback_ = false;

 public:
  RunJobClient(grpc::CompletionQueue* cq,
               std::function<bool()> shutdown_requested, connect_t connect)
      : connector_thunk_(this),
        reader_thunk_(this),
        writer_thunk_(this),
        cq_(cq),
        shutdown_requested_(std::move(shutdown_requested)),
        connect_(std::move(connect)) {}

  ~RunJobClient() { SPDLOG_TRACE("[0x{}] deleted", (void*)this); }

  // コピー、ムーブ禁止
  RunJobClient(const RunJobClient&) = delete;
  RunJobClient(RunJobClient&&) = delete;
  RunJobClient& operator=(const RunJobClient&) = delete;
  RunJobClient& operator=(RunJobClient&&) = delete;

  void SetOnConnect(on_connect_t on_connect) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_connect_ = std::move(on_connect);
  }
  void SetOnRead(on_read_t on_read) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_read_ = std::move(on_read);
  }
  void SetOnReadDone(on_read_done_t on_read_done) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_read_done_ = std::move(on_read_done);
  }
  void SetOnWritesDone(on_writes_done_t on_writes_done) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_writes_done_ = std::move(on_writes_done);
  }
  void SetOnError(on_error_t on_error) {
    std::lock_guard<std::mutex> guard(mutex_);
    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    on_error_ = std::move(on_error_);
  }

  void Connect() {
    std::lock_guard<std::mutex> guard(mutex_);
    read_status_ = ReadStatus::CONNECTING;
    write_status_ = WriteStatus::CONNECTING;
    streamer_ = connect_(&context_, cq_, &connector_thunk_);
  }

  void Shutdown() {
    std::unique_ptr<RunJobClient> p;
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_ || shutdown_requested_()) {
      return;
    }
    shutdown_ = true;

    on_connect_ = nullptr;
    on_read_ = nullptr;
    on_read_done_ = nullptr;
    on_writes_done_ = nullptr;
    on_error_ = nullptr;

    // 読み書き中だったらキャンセルされるまで待つ
    if (read_status_ == ReadStatus::CONNECTING ||
        read_status_ == ReadStatus::READING ||
        read_status_ == ReadStatus::FINISHING ||
        write_status_ == WriteStatus::CONNECTING ||
        write_status_ == WriteStatus::WRITING ||
        write_status_ == WriteStatus::FINISHING) {
      context_.TryCancel();
    } else {
      // そうでないなら即座に終わらせて良い
      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
    }
    if (Deletable()) {
      p.reset(this);
    }
  }

  void Write(cattleshed::RunJobRequest request) {
    std::lock_guard<std::mutex> guard(mutex_);
    SPDLOG_TRACE("[0x{}] Write: request={}", (void*)this,
                 request.DebugString());

    if (shutdown_ || shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      streamer_->Write(request, &writer_thunk_);
      write_status_ = WriteStatus::WRITING;
    } else if (write_status_ == WriteStatus::WRITING ||
               write_status_ == WriteStatus::CONNECTING) {
      request_queue_.push_back(std::shared_ptr<void>(
          new cattleshed::RunJobRequest(std::move(request))));
    }
  }

  void WritesDone() {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_ || shutdown_requested_()) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      streamer_->WritesDone(&writer_thunk_);
      write_status_ = WriteStatus::FINISHING;
    } else if (write_status_ == WriteStatus::WRITING ||
               write_status_ == WriteStatus::CONNECTING) {
      request_queue_.push_back(nullptr);
    }
  }

 private:
  template <class F, class... Args>
  void RunCallback(std::unique_lock<std::mutex>& lock, F f, Args&&... args) {
    // 普通にコールバックするとデッドロックの可能性があるので
    // unlock してからコールバックする。
    // 再度ロックした時に状態が変わってる可能性があるので注意すること。
    if (f) {
      running_callback_ = true;
      lock.unlock();
      try {
        f(std::forward<Args>(args)...);
        // f に自身の shared_ptr がバインドされていて、コールバック中に SetOn〜 で nullptr が渡されてた場合、
        // RunCallback を抜ける時に Shutdown が呼ばれる可能性があり、ロック中に Shutdown するとデッドロックするので
        // アンロック中に f を無効にする必要がある。
        f = nullptr;
        lock.lock();
        running_callback_ = false;
      } catch (...) {
        lock.lock();
        running_callback_ = false;
      }
    }
  }

  void ProceedToConnect(bool ok) {
    std::unique_ptr<RunJobClient> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("[0x{}] ProceedToConnect: ok={}", (void*)this, ok);

    if (shutdown_ || shutdown_requested_()) {
      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      SPDLOG_ERROR("connection error");

      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
      RunCallback(lock, on_error_, RunJobClientError::CONNECT);
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    RunCallback(lock, on_connect_);

    // 終了要求が来てたので読み込みをせずに終了
    if (shutdown_ || shutdown_requested_()) {
      read_status_ = ReadStatus::FINISHED;
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    // 読み込み
    streamer_->Read(&response_, &reader_thunk_);
    read_status_ = ReadStatus::READING;

    HandleRequestQueue();
  }

  void ProceedToRead(bool ok) {
    std::unique_ptr<RunJobClient> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("[0x{}] ProceedToRead: ok={}", (void*)this, ok);

    if (shutdown_ || shutdown_requested_()) {
      read_status_ = ReadStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      if (read_status_ == ReadStatus::READING) {
        // 正常に finish した可能性があるので Finish する
        streamer_->Finish(&grpc_status_, &reader_thunk_);
        read_status_ = ReadStatus::FINISHING;
      } else if (read_status_ == ReadStatus::FINISHING) {
        SPDLOG_ERROR("reading or finishing error");
        // 書き込み中ならキャンセルする
        if (write_status_ == WriteStatus::CONNECTING ||
            write_status_ == WriteStatus::WRITING ||
            write_status_ == WriteStatus::FINISHING) {
          context_.TryCancel();
        } else {
          write_status_ = WriteStatus::FINISHED;
        }
        read_status_ = ReadStatus::FINISHED;
        RunCallback(lock, on_error_, RunJobClientError::READ);
        if (Deletable()) {
          p.reset(this);
        }
      }
      return;
    }

    if (read_status_ == ReadStatus::READING) {
      // 結果が取得できた

      RunCallback(lock, on_read_, std::move(response_));

      // 終了要求が来てたので次の読み込みをせずに終了
      if (shutdown_ || shutdown_requested_()) {
        read_status_ = ReadStatus::FINISHED;
        if (Deletable()) {
          p.reset(this);
        }
        return;
      }

      // 次の読み込み
      streamer_->Read(&response_, &reader_thunk_);
      read_status_ = ReadStatus::READING;
    } else if (read_status_ == ReadStatus::FINISHING) {
      // 終了

      RunCallback(lock, on_read_done_, grpc_status_);

      if (grpc_status_.ok()) {
        SPDLOG_DEBUG("gRPC Read finished");
      } else {
        SPDLOG_ERROR("gRPC error: {} ({})", grpc_status_.error_message(),
                     grpc_status_.error_code());
        SPDLOG_ERROR("   details: {}", grpc_status_.error_details());
      }
      // 正常終了
      read_status_ = ReadStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
    }
  }

  void ProceedToWrite(bool ok) {
    std::unique_ptr<RunJobClient> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("[0x{}] ProceedToWrite: ok={}", (void*)this, ok);

    if (shutdown_ || shutdown_requested_()) {
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      // 読み込み中ならキャンセルする
      if (read_status_ == ReadStatus::CONNECTING ||
          read_status_ == ReadStatus::READING ||
          read_status_ == ReadStatus::FINISHING) {
        context_.TryCancel();
      } else {
        read_status_ = ReadStatus::FINISHED;
      }
      write_status_ = WriteStatus::FINISHED;
      RunCallback(lock, on_error_, RunJobClientError::WRITE);
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (write_status_ == WriteStatus::FINISHING) {
      RunCallback(lock, on_writes_done_);

      // 書き込み完了。
      // あとは読み込みが全て終了したら終わり。
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    // 書き込みが成功したら次のキューを処理する
    HandleRequestQueue();
  }

  void HandleRequestQueue() {
    if (request_queue_.empty()) {
      write_status_ = WriteStatus::IDLE;
    } else {
      auto ptr = request_queue_.front();
      request_queue_.pop_front();
      if (ptr != nullptr) {
        // 通常の書き込みリクエスト
        streamer_->Write(
            *std::static_pointer_cast<cattleshed::RunJobRequest>(ptr),
            &writer_thunk_);
        write_status_ = WriteStatus::WRITING;
      } else {
        // 完了のリクエスト
        streamer_->WritesDone(&writer_thunk_);
        write_status_ = WriteStatus::FINISHING;
      }
    }
  }

  bool Deletable() const {
    return (shutdown_ || shutdown_requested_()) && !running_callback_ &&
           read_status_ == ReadStatus::FINISHED &&
           write_status_ == WriteStatus::FINISHED;
  }
};

class CattleshedClientManager {
  std::mutex mutex_;

  struct ThreadData {
    grpc::CompletionQueue cq;
    std::unique_ptr<std::thread> thread;
  };
  std::vector<ThreadData> threads_;

  uint32_t next_client_id_ = 0;
  std::atomic_bool shutdown_{false};

  std::unique_ptr<cattleshed::Cattleshed::Stub> stub_;

 public:
  CattleshedClientManager(std::shared_ptr<grpc::Channel> channel, int threads)
      : threads_(threads), stub_(cattleshed::Cattleshed::NewStub(channel)) {
    for (auto& th : threads_) {
      th.thread.reset(new std::thread([& cq = th.cq]() {
        void* got_tag;
        bool ok = false;

        while (cq.Next(&got_tag, &ok)) {
          CattleshedClientHandler* call =
              static_cast<CattleshedClientHandler*>(got_tag);
          call->Proceed(ok);
        }
      }));
    }
  }

  ~CattleshedClientManager() { Shutdown(); }

  void Shutdown() {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_) {
      return;
    }
    shutdown_ = true;

    SPDLOG_TRACE("ClientManager::Shutdown started");

    // キューを Shutdown して、全てのスレッドが終了するのを待つ
    // コールバックの処理で無限ループしてるとかじゃない限りは終了するはず
    for (auto& th : threads_) {
      th.cq.Shutdown();
    }

    SPDLOG_TRACE("ClientManager::Shutdown released completion queue");

    for (auto& th : threads_) {
      th.thread->join();
    }

    SPDLOG_TRACE("ClientManager::Shutdown finished");
  }

  std::shared_ptr<GetVersionClient> CreateGetVersionClient() {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client_id = next_client_id_++;
    auto cq = &threads_[client_id % threads_.size()].cq;

    auto request = [stub = stub_.get()](
                       grpc::ClientContext* context,
                       const cattleshed::GetVersionRequest& req,
                       grpc::CompletionQueue* cq) {
      return stub->AsyncGetVersion(context, req, cq);
    };
    std::shared_ptr<GetVersionClient> p(
        new GetVersionClient(cq, [this]() -> bool { return shutdown_; },
                             std::move(request)),
        [](GetVersionClient* p) { p->Shutdown(); });
    return p;
  }

  std::shared_ptr<RunJobClient> CreateRunJobClient() {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client_id = next_client_id_++;
    auto cq = &threads_[client_id % threads_.size()].cq;

    auto connect = [stub = stub_.get()](grpc::ClientContext* context,
                                        grpc::CompletionQueue* cq, void* tag) {
      return stub->AsyncRunJob(context, cq, tag);
    };
    std::shared_ptr<RunJobClient> p(
        new RunJobClient(cq, [this]() -> bool { return shutdown_; },
                         std::move(connect)),
        [](RunJobClient* p) { p->Shutdown(); });
    return p;
  }
};

#endif // CATTLESHED_CLIENT_H_INCLUDED
