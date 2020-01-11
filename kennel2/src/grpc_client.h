#ifndef GRPC_CLIENT
#define GRPC_CLIENT

#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <sstream>
#include <thread>

#include <grpcpp/grpcpp.h>
#include <grpcpp/support/async_stream.h>
#include <grpcpp/support/async_unary_call.h>
#include <spdlog/spdlog.h>

struct GrpcHandler {
  virtual void Proceed(bool ok) = 0;
};

template <class W, class R>
class GrpcClientReaderWriterImpl {
  struct ConnectorThunk : GrpcHandler {
    GrpcClientReaderWriterImpl* p;
    ConnectorThunk(GrpcClientReaderWriterImpl* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToConnect(ok); }
  };
  ConnectorThunk connector_thunk_;
  friend class ConnectorThunk;

  struct ReaderThunk : GrpcHandler {
    GrpcClientReaderWriterImpl* p;
    ReaderThunk(GrpcClientReaderWriterImpl* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToRead(ok); }
  };
  ReaderThunk reader_thunk_;
  friend class ReaderThunk;

  struct WriterThunk : GrpcHandler {
    GrpcClientReaderWriterImpl* p;
    WriterThunk(GrpcClientReaderWriterImpl* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToWrite(ok); }
  };
  WriterThunk writer_thunk_;
  friend class WriterThunk;

  // ClientAsyncReaderWriter よりも ClientContext の方が寿命が長くなるようにしないといけないので、
  // 必ず streamer_ より上に context_ を定義すること
  grpc::ClientContext context_;
  std::unique_ptr<grpc::ClientAsyncReaderWriter<W, R>> streamer_;

  enum class ReadStatus { CONNECTING, READING, FINISHING, FINISHED };
  ReadStatus read_status_ = ReadStatus::CONNECTING;
  enum class WriteStatus { CONNECTING, WRITING, IDLE, FINISHING, FINISHED };
  WriteStatus write_status_ = WriteStatus::CONNECTING;

  std::deque<std::shared_ptr<void>> request_queue_;
  R response_;

  grpc::Status grpc_status_;

  bool shutdown_ = false;
  std::mutex mutex_;

  std::function<void(R)> read_;
  std::function<void(grpc::Status)> done_;

 public:
  GrpcClientReaderWriterImpl(
      std::function<std::unique_ptr<grpc::ClientAsyncReaderWriter<W, R>>(
          grpc::ClientContext*, void*)>
          async_connect,
      std::function<void(R)> read, std::function<void(grpc::Status)> done)
      : connector_thunk_(this),
        reader_thunk_(this),
        writer_thunk_(this),
        read_(std::move(read)),
        done_(std::move(done)) {
    std::lock_guard<std::mutex> guard(mutex_);
    streamer_ = async_connect(&context_, &connector_thunk_);
  }
  ~GrpcClientReaderWriterImpl() { SPDLOG_DEBUG("deleted: {}", (void*)this); }

  // コピー、ムーブ禁止
  GrpcClientReaderWriterImpl(const GrpcClientReaderWriterImpl&) = delete;
  GrpcClientReaderWriterImpl(GrpcClientReaderWriterImpl&&) = delete;
  GrpcClientReaderWriterImpl& operator=(const GrpcClientReaderWriterImpl&) =
      delete;
  GrpcClientReaderWriterImpl& operator=(GrpcClientReaderWriterImpl&&) = delete;

  void Shutdown() {
    std::unique_ptr<GrpcClientReaderWriterImpl> p;
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_) {
      return;
    }

    shutdown_ = true;

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

  void Write(W request) {
    std::lock_guard<std::mutex> guard(mutex_);
    SPDLOG_TRACE("Write: {}", (void*)this);

    if (shutdown_) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      streamer_->Write(request, &writer_thunk_);
      write_status_ = WriteStatus::WRITING;
    } else if (write_status_ == WriteStatus::WRITING ||
               write_status_ == WriteStatus::CONNECTING) {
      request_queue_.push_back(
          std::shared_ptr<void>(new W(std::move(request))));
    }
  }

  void WritesDone() {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_) {
      return;
    }

    if (write_status_ == WriteStatus::IDLE) {
      streamer_->WritesDone(&writer_thunk_);
      write_status_ = WriteStatus::FINISHING;
    } else if (write_status_ == WriteStatus::WRITING) {
      request_queue_.push_back(nullptr);
    }
  }

 private:
  void ProceedToConnect(bool ok) {
    std::unique_ptr<GrpcClientReaderWriterImpl> p;
    std::lock_guard<std::mutex> guard(mutex_);
    SPDLOG_TRACE("ProceedToConnect: {}", (void*)this);

    if (!ok) {
      SPDLOG_ERROR("connection error");
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
    std::unique_ptr<GrpcClientReaderWriterImpl> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("ProceedToRead: {}", (void*)this);

    if (shutdown_) {
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
        read_status_ = ReadStatus::FINISHED;
        if (Deletable()) {
          p.reset(this);
        }
      }
      return;
    }

    if (read_status_ == ReadStatus::READING) {
      // 結果が取得できた

      // 普通にコールバックするとデッドロックの可能性があるので
      // unlock してからコールバックする。
      // 再度ロックした時に状態が変わってる可能性があるので注意すること。
      lock.unlock();
      try {
        read_(std::move(response_));
        lock.lock();
      } catch (...) {
        lock.lock();
      }

      // 終了要求が来てたので次の読み込みをせずに終了
      if (shutdown_) {
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

      // 普通にコールバックするとデッドロックの可能性があるので
      // unlock してからコールバックする。
      // 再度ロックした時に状態が変わってる可能性があるので注意すること。
      lock.unlock();
      try {
        done_(std::move(grpc_status_));
        lock.lock();
      } catch (...) {
        lock.lock();
      }

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
    std::unique_ptr<GrpcClientReaderWriterImpl> p;
    std::lock_guard<std::mutex> guard(mutex_);
    SPDLOG_TRACE("ProceedToWrite: {}", (void*)this);

    if (shutdown_) {
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (!ok) {
      context_.TryCancel();
      write_status_ = WriteStatus::FINISHED;
      if (Deletable()) {
        p.reset(this);
      }
      return;
    }

    if (write_status_ == WriteStatus::FINISHING) {
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
        streamer_->Write(*std::static_pointer_cast<W>(ptr), &writer_thunk_);
        write_status_ = WriteStatus::WRITING;
      } else {
        // 完了のリクエスト
        streamer_->WritesDone(&writer_thunk_);
        write_status_ = WriteStatus::FINISHING;
      }
    }
  }

  bool Deletable() const {
    return shutdown_ && read_status_ == ReadStatus::FINISHED &&
           write_status_ == WriteStatus::FINISHED;
  }
};

template <class W, class R>
class GrpcClientReaderWriter {
  GrpcClientReaderWriterImpl<W, R>* p_;

 public:
  template <class... Args>
  GrpcClientReaderWriter(Args... args)
      : p_(new GrpcClientReaderWriterImpl<W, R>(std::forward<Args>(args)...)) {}

  ~GrpcClientReaderWriter() { Shutdown(); }

  // コピー、ムーブ禁止
  GrpcClientReaderWriter(const GrpcClientReaderWriter&) = delete;
  GrpcClientReaderWriter(GrpcClientReaderWriter&&) = delete;
  GrpcClientReaderWriter& operator=(const GrpcClientReaderWriter&) = delete;
  GrpcClientReaderWriter& operator=(GrpcClientReaderWriter&&) = delete;

  void Shutdown() { p_->Shutdown(); }
  void Write(W request) { p_->Write(std::move(request)); }

  void WritesDone() { p_->WritesDone(); }
};

template <class R>
class GrpcClientResponseReaderImpl {
  struct ReaderThunk : GrpcHandler {
    GrpcClientResponseReaderImpl* p;
    ReaderThunk(GrpcClientResponseReaderImpl* p) : p(p) {}
    void Proceed(bool ok) override { p->ProceedToRead(ok); }
  };
  ReaderThunk reader_thunk_;
  friend class ReaderThunk;

  // ClientAsyncResponseReader よりも ClientContext の方が寿命が長くなるようにしないといけないので、
  // 必ず reader_ より上に context_ を定義すること
  grpc::ClientContext context_;
  std::unique_ptr<grpc::ClientAsyncResponseReader<R>> reader_;

  R response_;
  grpc::Status grpc_status_;

  bool shutdown_ = false;
  std::mutex mutex_;

  std::function<void(R, grpc::Status)> done_;

 public:
  GrpcClientResponseReaderImpl(
      std::function<std::unique_ptr<grpc::ClientAsyncResponseReader<R>>( grpc::ClientContext*)> async_request, std::function<void(R, grpc::Status)> done)
      : reader_thunk_(this),
        done_(std::move(done)) {
    std::lock_guard<std::mutex> guard(mutex_);
    reader_ = async_request(&context_);
    reader_->Finish(&response_, &grpc_status_, &reader_thunk_);
  }
  ~GrpcClientResponseReaderImpl() { SPDLOG_DEBUG("deleted: {}", (void*)this); }

  // コピー、ムーブ禁止
  GrpcClientResponseReaderImpl(const GrpcClientResponseReaderImpl&) = delete;
  GrpcClientResponseReaderImpl(GrpcClientResponseReaderImpl&&) = delete;
  GrpcClientResponseReaderImpl& operator=(const GrpcClientResponseReaderImpl&) =
      delete;
  GrpcClientResponseReaderImpl& operator=(GrpcClientResponseReaderImpl&&) = delete;

  void Shutdown() {
    std::unique_ptr<GrpcClientResponseReaderImpl> p;
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_) {
      return;
    }

    shutdown_ = true;

    // キャンセルされるまで待つ
    context_.TryCancel();
  }

 private:
  void ProceedToRead(bool ok) {
    std::unique_ptr<GrpcClientResponseReaderImpl> p;
    std::unique_lock<std::mutex> lock(mutex_);
    SPDLOG_TRACE("ProceedToRead: {}", (void*)this);

    if (shutdown_) {
      SPDLOG_INFO("shutdown GrpcClientResponseReaderImpl");
      p.reset(this);
      return;
    }

    if (!ok) {
      SPDLOG_ERROR("finishing error");
      p.reset(this);
      return;
    }

    // 結果が取得できた

    // 普通にコールバックするとデッドロックの可能性があるので
    // unlock してからコールバックする。
    // 再度ロックした時に状態が変わってる可能性があるので注意すること。
    lock.unlock();
    try {
      done_(std::move(response_), std::move(grpc_status_));
      lock.lock();
    } catch (...) {
      lock.lock();
    }

    p.reset(this);
  }
};

template <class R>
class GrpcClientResponseReader {
  GrpcClientResponseReaderImpl<R>* p_;

 public:
  template <class... Args>
  GrpcClientResponseReader(Args... args)
      : p_(new GrpcClientResponseReaderImpl<R>(std::forward<Args>(args)...)) {}

  ~GrpcClientResponseReader() { Shutdown(); }

  // コピー、ムーブ禁止
  GrpcClientResponseReader(const GrpcClientResponseReader&) = delete;
  GrpcClientResponseReader(GrpcClientResponseReader&&) = delete;
  GrpcClientResponseReader& operator=(const GrpcClientResponseReader&) = delete;
  GrpcClientResponseReader& operator=(GrpcClientResponseReader&&) = delete;

  void Shutdown() { p_->Shutdown(); }
};

class GrpcClientManager;

class GrpcClient {
  uint32_t client_id_;
  GrpcClientManager* gcm_;

 public:
  GrpcClient(uint32_t client_id, GrpcClientManager* gcm)
      : client_id_(client_id), gcm_(gcm) {}
  ~GrpcClient();
};

template <class W>
class GrpcWritableClient {
  uint32_t client_id_;
  GrpcClientManager* gcm_;

 public:
  GrpcWritableClient(uint32_t client_id, GrpcClientManager* gcm)
      : client_id_(client_id), gcm_(gcm) {}
  ~GrpcWritableClient();

  void Write(W data);
  void WritesDone();
};

class GrpcClientManager {
  std::mutex mutex_;

  struct ThreadData {
    grpc::CompletionQueue cq;
    std::unique_ptr<std::thread> thread;
  };
  std::vector<ThreadData> threads_;

  struct Holder {
    virtual ~Holder() {}
    virtual void Shutdown() = 0;
  };

  template<class R>
  struct ResponseReaderHolderImpl : Holder {
    GrpcClientResponseReader<R> v;

    template <class... Args>
    ResponseReaderHolderImpl(Args... args) : v(std::forward<Args>(args)...) {}
    void Shutdown() override { v.Shutdown(); }
  };

  // 全ての GrpcClientReaderWriter<W, R> を同じ変数に入れたいので
  // type erasure イディオムを使う
  struct WritableHolder : Holder {
    virtual ~WritableHolder() {}
    virtual const std::type_info& WriterTypeInfo() const = 0;
    virtual void Write(void* p) = 0;
    virtual void WritesDone() = 0;
    virtual void Shutdown() = 0;
  };

  template <class W, class R>
  struct ReaderWriterHolderImpl : WritableHolder {
    GrpcClientReaderWriter<W, R> v;

    template <class... Args>
    ReaderWriterHolderImpl(Args... args) : v(std::forward<Args>(args)...) {}

    const std::type_info& WriterTypeInfo() const override { return typeid(W); }
    void Write(void* p) override { v.Write(std::move(*(W*)p)); }
    void WritesDone() override { v.WritesDone(); }
    void Shutdown() override { v.Shutdown(); }
  };

  uint32_t next_client_id_ = 0;
  std::map<uint32_t, std::unique_ptr<Holder>> clients_;
  std::map<uint32_t, std::unique_ptr<WritableHolder>> writable_clients_;
  bool shutdown_ = false;

 public:
  GrpcClientManager(int threads) : threads_(threads) {
    for (auto& th : threads_) {
      th.thread.reset(new std::thread([& cq = th.cq]() {
        void* got_tag;
        bool ok = false;

        while (cq.Next(&got_tag, &ok)) {
          GrpcHandler* call = static_cast<GrpcHandler*>(got_tag);
          call->Proceed(ok);
        }
      }));
    }
  }

  ~GrpcClientManager() { Shutdown(); }

  void Shutdown() {
    std::lock_guard<std::mutex> guard(mutex_);

    if (shutdown_) return;
    SPDLOG_TRACE("GrpcClientManager::Shutdown started");

    shutdown_ = true;

    // これで各接続の GrpcClientReaderWriter::Shutdown が呼ばれる。
    clients_.clear();
    writable_clients_.clear();

    // キューを Shutdown して、全てのスレッドが終了するのを待つ
    // コールバックの処理で無限ループしてるとかじゃない限りは終了するはず
    for (auto& th : threads_) {
      th.cq.Shutdown();
    }

    SPDLOG_TRACE("GrpcClientManager::Shutdown released completion queue");

    for (auto& th : threads_) {
      th.thread->join();
    }

    SPDLOG_TRACE("GrpcClientManager::Shutdown finished");
  }

  template <class W, class R>
  std::unique_ptr<GrpcWritableClient<W>> CreateReaderWriterClient(
      std::function<std::unique_ptr<grpc::ClientAsyncReaderWriter<W, R>>(
          grpc::ClientContext*, grpc::CompletionQueue*, void*)>
          async_connect,
      std::function<void(R)> read, std::function<void(grpc::Status)> done) {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client_id = next_client_id_++;

    // cq を bind して引数を減らす
    auto cq = &threads_[client_id % threads_.size()].cq;
    auto async_connect2 = [f = std::move(async_connect), cq](
                              grpc::ClientContext* client, void* tag) {
      return std::move(f)(client, cq, tag);
    };

    std::unique_ptr<WritableHolder> holder(new ReaderWriterHolderImpl<W, R>(
        std::move(async_connect2), std::move(read), std::move(done)));
    writable_clients_.insert(std::make_pair(client_id, std::move(holder)));
    return std::unique_ptr<GrpcWritableClient<W>>(
        new GrpcWritableClient<W>(client_id, this));
  }

  template <class W, class R>
  std::unique_ptr<GrpcClient> CreateClient(
      std::function<std::unique_ptr<grpc::ClientAsyncResponseReader<R>>(grpc::ClientContext*, const W&, grpc::CompletionQueue*)> async_request, const W& request, std::function<void(R, grpc::Status)> done) {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client_id = next_client_id_++;

    // cq と request を bind して引数を減らす
    auto cq = &threads_[client_id % threads_.size()].cq;
    auto async_request2 = [f = std::move(async_request), &request, cq](
                              grpc::ClientContext* client) {
      return std::move(f)(client, request, cq);
    };

    std::unique_ptr<Holder> holder(new ResponseReaderHolderImpl<R>(
        std::move(async_request2), std::move(done)));
    clients_.insert(std::make_pair(client_id, std::move(holder)));
    return std::unique_ptr<GrpcClient>(
        new GrpcClient(client_id, this));
  }

 private:
  WritableHolder* GetWritableClient(uint32_t client_id) {
    if (shutdown_) {
      return nullptr;
    }

    auto it = writable_clients_.find(client_id);
    if (it == writable_clients_.end()) {
      return nullptr;
    }

    return it->second.get();
  }

  // 以下は GrpcClient からのみアクセスさせる
  friend class GrpcClient;

  void DestroyClient(uint32_t client_id) {
    std::lock_guard<std::mutex> guard(mutex_);
    clients_.erase(client_id);
  }

  // 以下は GrpcWritableClient<W> からのみアクセスさせる
  template <class W>
  friend class GrpcWritableClient;

  void DestroyWritableClient(uint32_t client_id) {
    std::lock_guard<std::mutex> guard(mutex_);
    writable_clients_.erase(client_id);
  }

  template <class W>
  void Write(uint32_t client_id, W req) {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client = GetWritableClient(client_id);
    if (client == nullptr) {
      return;
    }
    // 書き込む型が一致してない
    if (client->WriterTypeInfo() != typeid(W)) {
      return;
    }

    client->Write(&req);
  }
  void WritesDone(uint32_t client_id) {
    std::lock_guard<std::mutex> guard(mutex_);

    auto client = GetWritableClient(client_id);
    if (client == nullptr) {
      return;
    }

    client->WritesDone();
  }
};

inline GrpcClient::~GrpcClient() {
  gcm_->DestroyClient(client_id_);
}

template <class W>
GrpcWritableClient<W>::~GrpcWritableClient() {
  gcm_->DestroyWritableClient(client_id_);
}
template <class W>
void GrpcWritableClient<W>::Write(W data) {
  gcm_->Write(client_id_, std::move(data));
}
template <class W>
void GrpcWritableClient<W>::WritesDone() {
  gcm_->WritesDone(client_id_);
}

#endif  // GRPC_CLIENT
