#ifndef CATTLESHED_CLIENT_H_
#define CATTLESHED_CLIENT_H_

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
    ggrpc::ClientResponseReader<wandbox::cattleshed::GetVersionRequest,
                                wandbox::cattleshed::GetVersionResponse>;
using RunJobClient =
    ggrpc::ClientReaderWriter<wandbox::cattleshed::RunJobRequest,
                              wandbox::cattleshed::RunJobResponse>;

class CattleshedClientManager {
  ggrpc::ClientManager cm_;
  std::unique_ptr<wandbox::cattleshed::Cattleshed::Stub> stub_;

 public:
  CattleshedClientManager(std::shared_ptr<grpc::Channel> channel, int threads)
      : cm_(threads), stub_(wandbox::cattleshed::Cattleshed::NewStub(channel)) {
    cm_.Start();
  }

  std::shared_ptr<GetVersionClient> CreateGetVersionClient() {
    auto request = [stub = stub_.get()](
                       grpc::ClientContext* context,
                       const wandbox::cattleshed::GetVersionRequest& req,
                       grpc::CompletionQueue* cq) {
      return stub->AsyncGetVersion(context, req, cq);
    };
    return cm_.CreateResponseReader<wandbox::cattleshed::GetVersionRequest,
                                    wandbox::cattleshed::GetVersionResponse>(
        request);
  }

  std::shared_ptr<RunJobClient> CreateRunJobClient() {
    auto connect = [stub = stub_.get()](grpc::ClientContext* context,
                                        grpc::CompletionQueue* cq, void* tag) {
      return stub->AsyncRunJob(context, cq, tag);
    };
    return cm_.CreateReaderWriter<wandbox::cattleshed::RunJobRequest,
                                  wandbox::cattleshed::RunJobResponse>(connect);
  }
};

#endif  // CATTLESHED_CLIENT_H_
