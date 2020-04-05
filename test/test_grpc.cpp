#include <thread>

#include <spdlog/spdlog.h>

#include "../kennel2/src/cattleshed_client.h"
#include "../kennel2/src/cattleshed_server.h"

int main() {
  spdlog::set_level(spdlog::level::trace);

  CattleshedServer server;
  server.Start("0.0.0.0:50051", 10);
  std::this_thread::sleep_for(std::chrono::seconds(1));

  auto channel = grpc::CreateChannel("127.0.0.1:50051",
                                     grpc::InsecureChannelCredentials());
  CattleshedClientManager cm(channel, 10);
  cattleshed::GetVersionRequest req;
  auto client = cm.CreateGetVersionClient();
  client->Request(req);
  auto client2 = cm.CreateRunJobClient();
  client2->Connect();
  cattleshed::RunJobRequest req2;
  client2->Write(req2);
  client2->WritesDone();
  std::this_thread::sleep_for(std::chrono::seconds(1));
}
