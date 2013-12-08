#include "syslogstream.hpp"
#include <syslog.h>
#include <iostream>

namespace wandbox {
	syslogstreambuf::syslogstreambuf(const char *ident, int option, int facility, int priority): buf(), pri(priority) {
		openlog(ident, option, facility);
	}
	syslogstreambuf::~syslogstreambuf() {
		sync();
		closelog();
	}
	int syslogstreambuf::overflow(int c) {
		if (c != traits_type::eof()) buf += (char)(c & 0xFF);
		return c;
	}
	int syslogstreambuf::sync() {
		if (!buf.empty()) syslog(pri, "%s", buf.c_str());
		buf.clear();
		return 0;
	}
}
