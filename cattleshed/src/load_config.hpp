#include <string>
#include <vector>
#include <istream>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/hashed_index.hpp>

namespace wandbox {
	namespace mendex = boost::multi_index;

	struct switch_trait {
		std::string name;
		std::vector<std::string> flags;
		bool default_;
		std::string display_name;
	};
	typedef mendex::multi_index_container<switch_trait, mendex::indexed_by<mendex::sequenced<>, mendex::hashed_unique<mendex::member<switch_trait, std::string, &switch_trait::name>>>> switch_set;
	struct compiler_trait {
		std::string name;
		std::string output_file;
		std::string language;
		std::vector<std::string> compile_command;
		std::vector<std::string> version_command;
		std::vector<std::string> run_command;
		switch_set switches;
		std::string source_suffix;
		std::string display_name;
		std::string display_compile_command;
		bool displayable;
	};
	typedef mendex::multi_index_container<compiler_trait, mendex::indexed_by<mendex::sequenced<>, mendex::hashed_unique<mendex::member<compiler_trait, std::string, &compiler_trait::name>>>> compiler_set;

	struct server_config {
		compiler_set compilers;
	};

	server_config load_config(std::istream &is);
}
