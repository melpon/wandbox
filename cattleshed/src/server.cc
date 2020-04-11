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
#include <syslog.h>
#include <time.h>

#include "cattleshed_server.h"
#include "load_config.hpp"
#include "posixapi.hpp"
#include "syslogstream.hpp"

int main(int argc, char** argv) try {
  using namespace wandbox;

  ::setlocale(LC_ALL, "C");

  spdlog::set_level(spdlog::level::trace);

  std::shared_ptr<std::streambuf> logbuf(std::clog.rdbuf(), [](void*) {});

  wandbox::server_config config;

  {
    namespace po = boost::program_options;

    std::vector<std::string> config_files{
        std::string(SYSCONFDIR) + "/cattleshed.conf",
        std::string(SYSCONFDIR) + "/cattleshed.conf.d"};
    {
      po::options_description opt("options");
      opt.add_options()("help,h", "show this help")(
          "config,c", po::value<std::vector<std::string>>(&config_files),
          "specify config file")("syslog", "use syslog for trace")(
          "verbose", "be verbose");

      po::variables_map vm;
      po::store(po::parse_command_line(argc, argv, opt), vm);
      po::notify(vm);

      if (vm.count("help")) {
        std::cout << opt << std::endl;
        return 0;
      }

      if (vm.count("syslog")) {
        logbuf.reset(
            new syslogstreambuf("cattleshed", LOG_PID, LOG_DAEMON, LOG_DEBUG));
        std::clog.rdbuf(logbuf.get());
      }
    }

    try {
      config = load_config(config_files);
    } catch (...) {
      std::clog << "failed to read config file(s), check existence or syntax."
                << std::endl;
      throw;
    }

    if (config.system.storedir[0] != '/') {
      std::clog << "storedir must be absolute." << std::endl;
      return 1;
    }

    if (config.system.basedir[0] != '/') {
      std::clog << "basedir must be absolute." << std::endl;
      return 1;
    }
  }

  auto ioc = std::make_shared<boost::asio::io_context>();

  CattleshedServer server(ioc, config);
  server.Start("0.0.0.0:" + std::to_string(config.system.listen_port), 1);

  boost::asio::executor_work_guard<boost::asio::io_context::executor_type>
      work = boost::asio::make_work_guard(ioc->get_executor());
  ioc->run();
  return 0;
} catch (std::exception& e) {
  std::clog << "fatal: " << e.what() << std::endl;
  return -1;
}
