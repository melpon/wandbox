#include <array>
#include <deque>
#include <functional>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>

// boost
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/system/system_error.hpp>

// Linux
#include <aio.h>
#include <locale.h>
#include <sys/eventfd.h>
#include <time.h>

// CLI11
#include <CLI/CLI.hpp>

// spdlog
#include <spdlog/spdlog.h>

#include "cattleshed_server.h"
#include "load_config.hpp"
#include "posixapi.hpp"

int main(int argc, char** argv) try {
  using namespace wandbox;

  ::setlocale(LC_ALL, "C");

  CLI::App app("cattleshed");

  spdlog::level::level_enum log_level = spdlog::level::info;
  auto log_level_map =
      std::vector<std::pair<std::string, spdlog::level::level_enum>>(
          {{"trace", spdlog::level::trace},
           {"debug", spdlog::level::debug},
           {"info", spdlog::level::info},
           {"warning", spdlog::level::warn},
           {"error", spdlog::level::err},
           {"critical", spdlog::level::critical},
           {"off", spdlog::level::off}});
  app.add_option("--log-level", log_level, "Log severity level threshold")
      ->transform(CLI::CheckedTransformer(log_level_map, CLI::ignore_case));

  std::vector<std::string> config_paths;
  app.add_option("-c,--config", config_paths, "config files or dirs")
      ->check(CLI::ExistingPath)
      ->required();

  try {
    app.parse(argc, argv);
  } catch (const CLI::ParseError& e) {
    return app.exit(e);
  }

  spdlog::set_level(log_level);

  wandbox::server_config config;

  try {
    config = load_config(config_paths);
  } catch (...) {
    SPDLOG_ERROR("failed to read config file(s), check existence or syntax.");
    throw;
  }

  if (config.system.storedir[0] != '/') {
    SPDLOG_ERROR("storedir must be absolute.");
    return 1;
  }

  if (config.system.basedir[0] != '/') {
    SPDLOG_ERROR("basedir must be absolute.");
    return 1;
  }

  auto ioc = std::make_shared<boost::asio::io_context>();

  CattleshedServer server(ioc, config);
  server.Start("0.0.0.0:" + std::to_string(config.system.listen_port), 1);

  boost::asio::executor_work_guard<boost::asio::io_context::executor_type>
      work = boost::asio::make_work_guard(ioc->get_executor());
  try {
    ioc->run();
  } catch (std::exception& e) {
    SPDLOG_ERROR("fatal: {}", e.what());
    std::quick_exit(1);
  } catch (...) {
    SPDLOG_ERROR("fatal");
    std::quick_exit(1);
  }
  return 0;
} catch (std::exception& e) {
  SPDLOG_ERROR("fatal: {}", e.what());
  return -1;
}
