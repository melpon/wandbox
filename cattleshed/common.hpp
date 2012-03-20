#define BOOST_ERROR_CODE_HEADER_ONLY
#include <iostream>
#include <memory>
#include <boost/asio.hpp>
#include <boost/spirit/home/qi.hpp>
#include <boost/spirit/home/phoenix.hpp>
#include <boost/spirit/home/karma.hpp>

namespace wandbox {

	namespace asio = boost::asio;
	namespace qi = boost::spirit::qi;
	namespace phx = boost::phoenix;
	namespace karma = boost::spirit::karma;
	namespace spirit = boost::spirit;
	using asio::ip::tcp;
	using std::placeholders::_1;
	using std::placeholders::_2;
	using std::placeholders::_3;

	enum struct compile_opt_shifts {
		none,
		O0,
		O1,
		O2,
		Os,
		Ofast,
		Wall,
		Wextra,
	};

	struct ctor_notifier {
		std::ostream &print(const char *text) { return std::cout << this << ' ' << text << std::endl; }
		std::ostream &print(const char *text, const void *p) { return std::cout << this << ' ' << text << ' ' << p << std::endl; }
		ctor_notifier() { print("default ctor"); }
		ctor_notifier(const ctor_notifier &o) { print("copy ctor from", &o); }
		ctor_notifier(ctor_notifier &&o) { print("move ctor from", &o); }
		ctor_notifier &operator =(const ctor_notifier &o) { print("copy assign from", &o); return *this; }
		ctor_notifier &operator =(ctor_notifier &&o) { print("move assign from", &o); return *this; }
		~ctor_notifier() { print("dtor"); }
	};

	static const short listen_port = 2012;
}
