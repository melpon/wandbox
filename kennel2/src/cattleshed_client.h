#ifndef CATTLESHED_CLIENT_H_INCLUDED
#define CATTLESHED_CLIENT_H_INCLUDED

#include <atomic>
#include <deque>
#include <functional>
#include <iostream>
#include <memory>
#include <thread>

// ggrpc
#include <ggrpc/ggrpc.h>

// spdlog
#include <spdlog/spdlog.h>

#include "cattleshed.grpc.pb.h"
#include "cattleshed.pb.h"

using GetVersionClient =
    ggrpc::ClientResponseReader<cattleshed::GetVersionRequest,
                                cattleshed::GetVersionResponse>;
using RunJobClient = ggrpc::ClientReaderWriter<cattleshed::RunJobRequest,
                                               cattleshed::RunJobResponse>;

class CattleshedClientManager {
  ggrpc::ClientManager cm_;
  std::unique_ptr<cattleshed::Cattleshed::Stub> stub_;

 public:
  CattleshedClientManager(std::shared_ptr<grpc::Channel> channel, int threads)
      : cm_(threads), stub_(cattleshed::Cattleshed::NewStub(channel)) {
    cm_.Start();
  }

  std::shared_ptr<GetVersionClient> CreateGetVersionClient() {
    auto request = [stub = stub_.get()](
                       grpc::ClientContext* context,
                       const cattleshed::GetVersionRequest& req,
                       grpc::CompletionQueue* cq) {
      return stub->AsyncGetVersion(context, req, cq);
    };
    return cm_.CreateResponseReader<cattleshed::GetVersionRequest,
                                    cattleshed::GetVersionResponse>(request);
  }

  std::shared_ptr<RunJobClient> CreateRunJobClient() {
    auto connect = [stub = stub_.get()](grpc::ClientContext* context,
                                        grpc::CompletionQueue* cq, void* tag) {
      return stub->AsyncRunJob(context, cq, tag);
    };
    return cm_.CreateReaderWriter<cattleshed::RunJobRequest,
                                  cattleshed::RunJobResponse>(connect);
  }
};

#endif // CATTLESHED_CLIENT_H_INCLUDED
