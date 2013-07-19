#include "common.hpp"
#include "yield.hpp"
#include <map>
#include <string>
#include <sstream>
#include <vector>
#include <boost/lexical_cast.hpp>

namespace wandbox {

	using std::shared_ptr;
	using std::unique_ptr;
	
}

int main(int, char **) {
	using namespace wandbox;
	asio::io_service aio;

    tcp::resolver resolver(aio);
	tcp::resolver::query query(tcp::v4(), "localhost", boost::lexical_cast<std::string>(listen_port));
    tcp::resolver::iterator iterator = resolver.resolve(query);

    tcp::socket s(aio);
    asio::connect(s, iterator);

	const std::vector<std::pair<std::string, std::string>> requests = {
		{"Control", "compiler=gcc-4.8.1"},
		{"CompilerOption", "optimize"},
		{"CompilerOption", "C++11"},
		{"Source",
			"#include <stdio.h>\n"
			"#include <unistd.h>\n"
			"#include <errno.h>\n"
			"#include <string.h>\n"
			"#include <stdlib.h>\n"
			"int main() {\n"
			"	puts(\"child process start\");\n"
			"	fflush(stdout);\n"
			"	system(\"echo hogeeeeeeeeeee\");"
			"	perror(\"\");\n"
			"	puts(\"child process finish\");\n"
			"}\n" },
		{"Control", "run"}};
	std::stringstream request;
	for (const auto &r: requests) {
		const auto s = encode_qp(r.second);
		request << r.first << ' ' << s.length() << ':' << s << '\n';
	}
	const auto r = request.str();
	std::cout << r << std::endl;
	asio::write(s, asio::buffer(r));

	asio::streambuf reply;
	boost::system::error_code ec;
	asio::read(s, reply, ec);
	std::cout << "Reply is: ";
	const auto b = reply.data();
	const auto p = asio::buffer_cast<const char *>(b);
	const auto l = asio::buffer_size(b);
	std::cout << decode_qp(boost::make_iterator_range(p, p+l)) << std::flush;
}
