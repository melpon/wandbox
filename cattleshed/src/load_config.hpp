#include <string>
#include <vector>
#include <unordered_set>
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

	struct network_config {
		int listen_port;
	};

	struct jail_config {
		std::string exe;
		std::string basedir;
		int max_address_space;
		int max_cpu_time;
		int max_data_segment;
		int max_file_size;
		int max_open_file;
		int nice;
		std::unordered_set<std::string> allow_file_exact;
		std::unordered_set<std::string> allow_file_prefix;
	};

	struct server_config {
		network_config network;
		jail_config jail;
		compiler_set compilers;
	};

	server_config load_config(std::istream &is);
}
