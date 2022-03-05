#ifndef KENNEL_SERVER_H_
#define KENNEL_SERVER_H_

#include <string>

// spdlog
#include <spdlog/spdlog.h>

// Boost
#include <boost/asio.hpp>

#include "cattleshed_client.h"
#include "kennel.json.h"
#include "kennel_shared_data.h"
#include "kennel_session.h"

struct KennelServerConfig {
  boost::asio::ip::tcp::endpoint endpoint;
  std::shared_ptr<CattleshedClientManager> cm;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> initial_cattleshed_info;
  std::shared_ptr<wandbox::kennel::SponsorFile> sponsor_file;
  std::string database;
  std::string url;
};

class KennelServer : public std::enable_shared_from_this<KennelServer> {
  KennelServer(boost::asio::io_context& ioc, KennelServerConfig config)
      : acceptor_(ioc), socket_(ioc), config_(std::move(config)) {
    boost::system::error_code ec;

    // Open the acceptor
    acceptor_.open(config_.endpoint.protocol(), ec);
    if (ec) {
      SPDLOG_ERROR("Failed to open: {}", ec.message());
      return;
    }

    // Allow address reuse
    acceptor_.set_option(boost::asio::socket_base::reuse_address(true), ec);
    if (ec) {
      SPDLOG_ERROR("Failed to set_option: {}", ec.message());
      return;
    }

    // Bind to the server address
    acceptor_.bind(config_.endpoint, ec);
    if (ec) {
      SPDLOG_ERROR("Failed to bind: {}", ec.message());
      return;
    }

    // Start listening for connections
    acceptor_.listen(boost::asio::socket_base::max_listen_connections, ec);
    if (ec) {
      SPDLOG_ERROR("Failed to listen: {}", ec.message());
      return;
    }

    KennelSharedDataConfig sdconfig;
    sdconfig.cm = config_.cm;
    sdconfig.ioc = &ioc;
    sdconfig.ioc = &ioc;
    sdconfig.initial_cattleshed_info = config_.initial_cattleshed_info;
    shared_data_.reset(new KennelSharedData(std::move(sdconfig)));
    initialized_ = true;
  }

 public:
  static std::shared_ptr<KennelServer> Create(boost::asio::io_context& ioc,
                                              KennelServerConfig config) {
    auto p =
        std::shared_ptr<KennelServer>(new KennelServer(ioc, std::move(config)));
    if (!p->initialized_) {
      return nullptr;
    }

    return p;
  }

  void Run() { DoAccept(); }

 private:
  void DoAccept() {
    acceptor_.async_accept(
        socket_, std::bind(&KennelServer::OnAccept, shared_from_this(),
                           std::placeholders::_1));
  }
  void OnAccept(boost::system::error_code ec) {
    if (ec) {
      SPDLOG_ERROR("Failed to accept: {}", ec.message());
    } else {
      KennelSessionConfig config;
      config.cm = config_.cm;
      config.sd = shared_data_;
      config.sponsor_file = config_.sponsor_file;
      config.database = config_.database;
      config.url = config_.url;
      KennelSession::Create(std::move(socket_), std::move(config))->Run();
    }

    DoAccept();
  }

 private:
  boost::asio::ip::tcp::acceptor acceptor_;
  boost::asio::ip::tcp::socket socket_;

  KennelServerConfig config_;
  std::shared_ptr<KennelSharedData> shared_data_;
  bool initialized_ = false;
};

#endif // KENNEL_SERVER_H_