#ifndef JSONIF_NOTHROW_H_
#define JSONIF_NOTHROW_H_

// Boost
#include <boost/json.hpp>

#include <string>
#include <exception>

namespace jsonif {

// 例外を投げないバージョンの from_json と to_json
template <class T>
inline T from_json(const std::string& s, std::exception_ptr& e) {
  e = std::exception_ptr();
  try {
    return boost::json::value_to<T>(boost::json::parse(s));
  } catch (...) {
    e = std::current_exception();
    return T();
  }
}

template <class T>
inline std::string to_json(const T& v, std::exception_ptr& e) {
  e = std::exception_ptr();
  try {
    return boost::json::serialize(boost::json::value_from(v));
  } catch (...) {
    e = std::current_exception();
    return "";
  }
}

}  // namespace jsonif

#endif // JSONIF_NOTHROW_H_