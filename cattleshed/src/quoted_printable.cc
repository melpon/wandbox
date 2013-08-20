#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/karma.hpp>

#include "quoted_printable.hpp"

namespace wandbox {
namespace quoted_printable {

	namespace qi = boost::spirit::qi;
	namespace karma = boost::spirit::karma;

	std::string decode(const std::string &r) {
		auto ite = r.begin();
		std::string ret;
		qi::parse(ite, r.end(), *(qi::omit[qi::lit('\n')] | (qi::char_ - '=') | (qi::lit("=\n")) | (qi::lit('=') > qi::uint_parser<char, 16, 2, 2>())), ret);
		return ret;
	}
	std::string encode(const std::string &r) {
		if (r.begin() == r.end()) return {};
		std::string ret;
		karma::generate(back_inserter(ret), karma::repeat(1, 76)[&karma::char_('=') << "=3D" | karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
		return ret;
	}
}
}
