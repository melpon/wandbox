#ifndef JSONIF_SERIALIZER_H_
#define JSONIF_SERIALIZER_H_

#include <string>

// Boost
#include <boost/json.hpp>

#include "kennel.json.h"

// 特殊すぎて自動生成のシリアライズ/デシリアライズでは対応できなかった型の
// シリアライズ/デシリアライズ処理を書く

namespace wandbox {
namespace kennel {

// ::wandbox::kennel::Switch
static void tag_invoke(const boost::json::value_from_tag&,
                       boost::json::value& jv,
                       const ::wandbox::kennel::Switch& v) {
  boost::json::object obj;
  obj["type"] = boost::json::value_from(v.type);
  obj["name"] = boost::json::value_from(v.name);
  if (v.display_name != decltype(v.display_name)()) {
    obj["display-name"] = boost::json::value_from(v.display_name);
  }
  if (v.display_flags != decltype(v.display_flags)()) {
    obj["display-flags"] = boost::json::value_from(v.display_flags);
  }
  if (v.options != decltype(v.options)()) {
    obj["options"] = boost::json::value_from(v.options);
  }

  if (v.type == "single") {
    obj["default"] = boost::json::value_from(v.single_default);
  } else {
    obj["default"] = boost::json::value_from(v.select_default);
  }

  jv = std::move(obj);
}

static ::wandbox::kennel::Switch tag_invoke(
    const boost::json::value_to_tag<::wandbox::kennel::Switch>&,
    const boost::json::value& jv) {
  ::wandbox::kennel::Switch v;
  if (jv.as_object().find("type") != jv.as_object().end()) {
    v.type = boost::json::value_to<std::string>(jv.at("type"));
  }
  if (jv.as_object().find("name") != jv.as_object().end()) {
    v.name = boost::json::value_to<std::string>(jv.at("name"));
  }
  if (jv.as_object().find("display-name") != jv.as_object().end()) {
    v.display_name = boost::json::value_to<std::string>(jv.at("display-name"));
  }
  if (jv.as_object().find("display-flags") != jv.as_object().end()) {
    v.display_flags =
        boost::json::value_to<std::string>(jv.at("display-flags"));
  }
  if (jv.as_object().find("options") != jv.as_object().end()) {
    v.options = boost::json::value_to<
        std::vector<::wandbox::kennel::SelectSwitchOption>>(jv.at("options"));
  }
  if (jv.as_object().find("default") != jv.as_object().end()) {
    if (v.type == "single") {
      v.single_default = boost::json::value_to<bool>(jv.at("default"));
    } else {
      v.select_default = boost::json::value_to<std::string>(jv.at("default"));
    }
  }
  return v;
}

}  // namespace kennel
}  // namespace wandbox

#endif // JSONIF_SERIALIZER_H_