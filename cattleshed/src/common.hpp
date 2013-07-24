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
	string encode_qp(const Range &r) {
		if (r.begin() == r.end()) return {};
		string ret;
		karma::generate(back_inserter(ret), karma::repeat(1, 76)[&karma::char_('=') << "=3D" | karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
		return ret;
	}
}
