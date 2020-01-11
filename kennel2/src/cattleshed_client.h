#ifndef CATTLESHED_CLIENT_H_INCLUDED
#define CATTLESHED_CLIENT_H_INCLUDED

#include <functional>
#include <iostream>
#include <memory>
#include <thread>

#include "grpc_client.h"

#include "cattleshed.grpc.pb.h"
#include "cattleshed.pb.h"

using CattleshedGetVersionClient = std::unique_ptr<GrpcClient>;
using CattleshedRunJobClient = std::unique_ptr<GrpcWritableClient<cattleshed::RunJobRequest>>;

class CattleshedClientManager {
  std::unique_ptr<GrpcClientManager> gcm_;
  std::unique_ptr<cattleshed::Cattleshed::Stub> stub_;

 public:
  CattleshedClientManager(std::shared_ptr<grpc::Channel> channel, int threads)
      : stub_(cattleshed::Cattleshed::NewStub(channel)),
        gcm_(new GrpcClientManager(threads)) {}

  ~CattleshedClientManager() {
    SPDLOG_TRACE("CattleshedClientManager::~CattleshedClientManager");
  }

  void Shutdown() {
    SPDLOG_TRACE("CattleshedClientManager::Shutdown");
    gcm_->Shutdown();
  }

  CattleshedGetVersionClient GetVersion(const cattleshed::GetVersionRequest& request, std::function<void(cattleshed::GetVersionResponse, grpc::Status)> done) {
    auto async_request = [stub = stub_.get()](grpc::ClientContext* context, const cattleshed::GetVersionRequest& request, grpc::CompletionQueue* cq) {
      return stub->AsyncGetVersion(context, request, cq);
    };
    return gcm_->CreateClient<cattleshed::GetVersionRequest, cattleshed::GetVersionResponse>(
        std::move(async_request), request, std::move(done));
  }

  CattleshedRunJobClient RunJob(std::function<void(cattleshed::RunJobResponse)> read, std::function<void(grpc::Status)> done) {
    auto async_connect = [stub = stub_.get()](grpc::ClientContext* context,
                                              grpc::CompletionQueue* cq,
                                              void* tag) {
      return stub->AsyncRunJob(context, cq, tag);
    };
    return gcm_->CreateReaderWriterClient<cattleshed::RunJobRequest,
                                          cattleshed::RunJobResponse>(
        std::move(async_connect), std::move(read), std::move(done));
  }
};

#endif  // CATTLESHED_CLIENT_H_INCLUDED
