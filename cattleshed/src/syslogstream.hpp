#ifndef SYSLOGSTREAM_HPP_
#define SYSLOGSTREAM_HPP_

#include <streambuf>
#include <string>

namespace wandbox {
class syslogstreambuf : public std::streambuf {
 public:
  syslogstreambuf(const char* ident, int option, int facility, int priority);
  virtual ~syslogstreambuf();

 protected:
  int overflow(int c);
  int sync();

 private:
  std::string buf;
  int pri;
};
}  // namespace wandbox

#endif
