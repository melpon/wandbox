#include "common.hpp"
#include "yield.hpp"
#include <map>
#include <string>
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

	std::string request =
		"Compiler 3:g++\n"
		"CompilerOption 11:<optimize>2\n"
		"Source 48:#include<stdio.h>=0Aint main(){putchar('a');}=0A\n"
		"Control 3:run\n";
	asio::write(s, asio::buffer(request));

	asio::streambuf reply;
	boost::system::error_code ec;
	asio::read(s, reply, ec);
	std::cout << "Reply is: ";
	std::cout << &reply;
	std::cout << "\n";
}
