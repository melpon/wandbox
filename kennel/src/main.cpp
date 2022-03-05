#include <future>
#include <string>

// spdlog
#include <spdlog/spdlog.h>

// Boost
#include <boost/asio.hpp>

// CLI11
#include <CLI/CLI.hpp>

#include "cattleshed_client.h"
#include "kennel.json.h"
#include "permlink.h"
#include "kennel_server.h"

int main(int argc, char* argv[]) {
  CLI::App app("Kennel - Wandbox API Server");

  std::string host = "127.0.0.1";
  int port = 3500;
  std::string cattleshed_host = "127.0.0.1";
  int cattleshed_port = 50051;
  std::string sponsor_json = "./sponsors.json";
  std::string database = "sqlite3:db=kennel.sqlite;@pool_size=10";
  std::string url = "http://localhost:8787";
  int log_level = spdlog::level::info;

  auto log_level_map = std::vector<std::pair<std::string, int>>(
      {{"trace", spdlog::level::trace},
       {"debug", spdlog::level::debug},
       {"warning", spdlog::level::warn},
       {"error", spdlog::level::err},
       {"critical", spdlog::level::critical},
       {"off", spdlog::level::off}});

  app.add_option("--host", host, "Listen host");
  app.add_option("--port", port, "Listen port");
  app.add_option("--cattleshed-host", cattleshed_host, "Cattleshed host");
  app.add_option("--cattleshed-port", cattleshed_port, "Cattleshed port");
  app.add_option("--sponsorsfile", sponsor_json, "Sponsors file");
  app.add_option("--database", database, "Database URI");
  app.add_option("--url", url, "Public URL for Wandbox");
  app.add_option("--log-level", log_level, "Log severity level threshold")
      ->transform(CLI::CheckedTransformer(log_level_map, CLI::ignore_case));

  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    exit(app.exit(e));
  }

  spdlog::set_level((spdlog::level::level_enum)log_level);

  wandbox::kennel::SponsorFile sponsor_file;
  if (sponsor_json.empty()) {
    SPDLOG_INFO("Sponsor file not set");
  } else {
    std::ifstream ifs(sponsor_json.c_str());
    if (!ifs) {
      SPDLOG_ERROR("Sponsor file {} not found", sponsor_json);
      return -1;
    }

    std::stringstream ss;
    ss << ifs.rdbuf();

    try {
      sponsor_file = jsonif::from_json<wandbox::kennel::SponsorFile>(ss.str());
    } catch (...) {
      SPDLOG_ERROR("Sponsor file {} parse error", sponsor_json);
      return -1;
    }
    SPDLOG_INFO("Sponsor file {} load completed", sponsor_json);
  }

  auto channel = grpc::CreateChannel(
      cattleshed_host + ":" + std::to_string(cattleshed_port),
      grpc::InsecureChannelCredentials());
  SPDLOG_INFO("Create gRPC channel to {}",
              cattleshed_host + ":" + std::to_string(cattleshed_port));
  std::shared_ptr<CattleshedClientManager> cm(
      new CattleshedClientManager(channel, 1));

  // DB の初期化
  permlink pl(database);
  pl.init();

  // cattleshed の GetVersion を呼んで、無事結果を取得できたらサーバーを起動する
  std::shared_ptr<wandbox::kennel::CattleshedInfo> info;
  {
    auto promise =
        std::make_shared<std::promise<wandbox::kennel::CattleshedInfo>>();
    auto future = promise->get_future();

    auto client = cm->CreateGetVersionClient();
    client->SetOnFinish(
        [client, promise](wandbox::cattleshed::GetVersionResponse resp,
                          grpc::Status status) mutable {
          if (!status.ok()) {
            SPDLOG_ERROR("Failed to GetVersion: {}, {}",
                         (int)status.error_code(), status.error_message());
            promise->set_exception(std::make_exception_ptr("error"));
            return;
          }
          promise->set_value(version_response_to_kennel(resp));
        });
    client->SetOnError(
        [promise](ggrpc::ClientResponseReaderError error) mutable {
          SPDLOG_ERROR("Failed to GetVersion (OnError): {}", (int)error);
          promise->set_exception(std::make_exception_ptr("error"));
        });
    wandbox::cattleshed::GetVersionRequest req;
    client->Connect(req);
    auto status = future.wait_for(std::chrono::seconds(60));
    if (status != std::future_status::ready) {
      SPDLOG_ERROR("Failed to Wait GetVersion");
      return -1;
    }
    info.reset(new wandbox::kennel::CattleshedInfo(future.get()));
  }

  boost::asio::io_context ioc{1};
  KennelServerConfig config;
  config.endpoint = boost::asio::ip::tcp::endpoint(
      boost::asio::ip::make_address(host), static_cast<unsigned short>(port));
  config.cm = cm;
  config.initial_cattleshed_info = info;
  config.sponsor_file =
      std::make_shared<wandbox::kennel::SponsorFile>(std::move(sponsor_file));
  config.database = std::move(database);
  config.url = std::move(url);
  SPDLOG_INFO("Start to listen {}:{}", host, port);
  KennelServer::Create(ioc, std::move(config))->Run();
  ioc.run();
}