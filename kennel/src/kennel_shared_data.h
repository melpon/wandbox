#ifndef KENNEL_SHARED_DATA_H_
#define KENNEL_SHARED_DATA_H_

#include <memory>

// Boost
#include <boost/asio.hpp>

#include "cattleshed_client.h"
#include "kennel.json.h"

// wandbox::cattleshed::GetVersionResponse を頑張って wandbox::kennel::CattleshedInfo に変換する
static wandbox::kennel::CattleshedInfo version_response_to_kennel(
    const wandbox::cattleshed::GetVersionResponse& resp) {
  wandbox::kennel::CattleshedInfo r;
  for (int i = 0; i < resp.compiler_info_size(); i++) {
    wandbox::kennel::CompilerInfo info;

    const wandbox::cattleshed::CompilerInfo& c = resp.compiler_info(i);
    info.name = c.name();
    info.version = c.version();
    info.language = c.language();
    info.display_name = c.display_name();
    for (int j = 0; j < c.templates_size(); j++) {
      info.templates.push_back(c.templates(j));
    }
    info.compiler_option_raw = c.compiler_option_raw();
    info.runtime_option_raw = c.runtime_option_raw();
    info.display_compile_command = c.display_compile_command();
    for (int j = 0; j < c.switches_size(); j++) {
      wandbox::kennel::Switch sw;

      const wandbox::cattleshed::Switch& s = c.switches(j);
      if (s.data_case() == wandbox::cattleshed::Switch::kSingle) {
        const wandbox::cattleshed::SingleSwitch& ss = s.single();
        sw.type = "single";
        sw.name = ss.name();
        sw.single_default = ss.default_value();
        sw.display_name = ss.display_name();
        sw.display_flags = ss.display_flags();
      } else {
        const wandbox::cattleshed::SelectSwitch& ss = s.select();
        sw.type = "select";
        sw.name = ss.name();
        sw.select_default = ss.default_value();
        for (int k = 0; k < ss.options_size(); k++) {
          wandbox::kennel::SelectSwitchOption option;
          const wandbox::cattleshed::SelectSwitchOption& opt = ss.options(k);
          option.name = opt.name();
          option.display_name = opt.display_name();
          option.display_flags = opt.display_flags();
          sw.options.push_back(std::move(option));
        }
      }

      info.switches.push_back(std::move(sw));
    }

    r.compilers.push_back(std::move(info));
  }

  for (int i = 0; i < resp.templates_size(); i++) {
    wandbox::kennel::Template tmpl;
    {
      const wandbox::cattleshed::Template& t = resp.templates(i);
      tmpl.name = t.name();
      tmpl.code = t.default_source();
      for (int j = 0; j < t.sources_size(); j++) {
        wandbox::kennel::Code code;
        code.file = t.sources(j).file_name();
        code.code = t.sources(j).source();
        tmpl.codes.push_back(std::move(code));
      }
      tmpl.stdin = t.stdin();
      tmpl.options = t.compiler_options();
      tmpl.compiler_option_raw = t.compiler_option_raw();
      tmpl.runtime_option_raw = t.runtime_option_raw();
    }
    r.templates.push_back(std::move(tmpl));
  }

  return r;
}

struct KennelSharedDataConfig {
  boost::asio::io_context* ioc;
  std::shared_ptr<CattleshedClientManager> cm;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> initial_cattleshed_info;
};

// すべての接続で共有するデータ
class KennelSharedData : public std::enable_shared_from_this<KennelSharedData> {
 public:
  KennelSharedData(KennelSharedDataConfig config) : config_(std::move(config)) {
    cattleshed_info_ = config_.initial_cattleshed_info;
  }

  void MaybeUpdateCattleshedInfo() {
    auto now = std::chrono::steady_clock::now();
    auto elapsed = now - last_updated_;
    if (elapsed <= std::chrono::hours(1)) {
      return;
    }
    last_updated_ = now;

    auto client = config_.cm->CreateGetVersionClient();
    client->SetOnFinish([self = shared_from_this(), client](
                            wandbox::cattleshed::GetVersionResponse resp,
                            grpc::Status status) {
      if (!status.ok()) {
        return;
      }
      boost::asio::post(*self->config_.ioc, [self, resp = std::move(resp)]() {
        self->cattleshed_info_.reset(new wandbox::kennel::CattleshedInfo(
            version_response_to_kennel(resp)));
      });
    });
    wandbox::cattleshed::GetVersionRequest req;
    client->Connect(req);
  }

  // 必要になったらロックを掛けてコピーする実装に変える
  std::shared_ptr<wandbox::kennel::CattleshedInfo> GetCattleshedInfo() const {
    return cattleshed_info_;
  }

 private:
  std::chrono::steady_clock::time_point last_updated_;
  KennelSharedDataConfig config_;
  std::shared_ptr<wandbox::kennel::CattleshedInfo> cattleshed_info_;
};


#endif // KENNEL_SHARED_DATA_H_