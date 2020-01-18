#ifndef CATTLESHED_CLIENT_H_INCLUDED
#define CATTLESHED_CLIENT_H_INCLUDED

#include <functional>
#include <iostream>
#include <memory>
#include <thread>

// ggrpc
#include <ggrpc/client.h>

#include "cattleshed.grpc.pb.h"
#include "cattleshed.pb.h"

class CattleshedClientManager {
  std::unique_ptr<ggrpc::ClientManager> gcm_;
  std::unique_ptr<cattleshed::Cattleshed::Stub> stub_;

public:
  CattleshedClientManager(std::shared_ptr<grpc::Channel> channel, int threads)
      : stub_(cattleshed::Cattleshed::NewStub(channel)),
        gcm_(new ggrpc::ClientManager(threads)) {}

  ~CattleshedClientManager() {
    SPDLOG_TRACE("CattleshedClientManager::~CattleshedClientManager");
  }

  void Shutdown() {
    SPDLOG_TRACE("CattleshedClientManager::Shutdown");
    gcm_->Shutdown();
  }

  std::unique_ptr<ggrpc::Client> GetVersion(
      const cattleshed::GetVersionRequest &request,
      std::function<void(cattleshed::GetVersionResponse, grpc::Status)> done,
      std::function<void(ggrpc::ClientResponseReaderError)> on_error) {
    auto async_request =
        [stub = stub_.get()](grpc::ClientContext *context,
                             const cattleshed::GetVersionRequest &request,
                             grpc::CompletionQueue *cq) {
          return stub->AsyncGetVersion(context, request, cq);
        };
    return gcm_->CreateClient<cattleshed::GetVersionRequest,
                              cattleshed::GetVersionResponse>(
        std::move(async_request), request, std::move(done),
        std::move(on_error));
  }

  std::unique_ptr<ggrpc::WritableClient<cattleshed::RunJobRequest>>
  RunJob(std::function<void(cattleshed::RunJobResponse)> read,
         std::function<void(grpc::Status)> done,
         std::function<void(ggrpc::ClientReaderWriterError)> on_error) {
    auto async_connect = [stub = stub_.get()](grpc::ClientContext *context,
                                              grpc::CompletionQueue *cq,
                                              void *tag) {
      return stub->AsyncRunJob(context, cq, tag);
    };
    return gcm_->CreateReaderWriterClient<cattleshed::RunJobRequest,
                                          cattleshed::RunJobResponse>(
        std::move(async_connect), std::move(read), std::move(done),
        std::move(on_error));
  }
};

#endif // CATTLESHED_CLIENT_H_INCLUDED
