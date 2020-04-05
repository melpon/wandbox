#ifndef QUOTED_PRINTABLE_HPP_
#define QUOTED_PRINTABLE_HPP_

#include <string>

namespace wandbox {
namespace quoted_printable {
std::string decode(const std::string& r);
std::string encode(const std::string& r);
}  // namespace quoted_printable
}  // namespace wandbox

#endif
