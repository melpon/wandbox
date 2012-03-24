#define BOOST_ERROR_CODE_HEADER_ONLY
#include <iostream>
#include <memory>
#include <string>
#include <boost/range.hpp>
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
	using std::string;

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

	extern void *enabler;

	template <typename Range, typename std::enable_if<std::is_convertible<typename boost::range_category<Range>::type, std::bidirectional_iterator_tag>::value>::type *& = enabler>
	string decode_qp(const Range &r) {
		auto ite = r.begin();
		const auto end = r.end();
		string ret;
		qi::parse(ite, end, *(qi::omit[qi::lit('\n')] | (qi::char_ - '=') | (qi::lit("=\n")) | (qi::lit('=') > qi::uint_parser<char, 16, 2, 2>())), ret);
		return ret;
	}
	template <typename Range>
//	auto encode_qp(const Range &r) -> typename std::enable_if<std::is_convertible<typename std::iterator_traits<decltype(boost::begin(r))>::iterator_category, std::bidirectional_iterator_tag>::value, string>::type {
	string encode_qp(const Range &r) {
		if (r.begin() == r.end()) return {};
		string ret;
		karma::generate(back_inserter(ret), karma::repeat(1, 76)[&karma::char_('=') << "=3D" | karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
		return ret;
	}
}
