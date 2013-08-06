#ifndef LOAD_CONFIG_HPP_
#define LOAD_CONFIG_HPP_

#include <functional>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
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
		std::string display_name;
		std::vector<std::string> conflicts;
		bool runtime;
		int insert_position;
	};
	struct compiler_trait {
		std::string name;
		std::string language;
		std::vector<std::string> compile_command;
		std::vector<std::string> version_command;
		std::vector<std::string> run_command;
		std::string source_suffix;
		std::string display_name;
		std::string display_compile_command;
		std::vector<std::string> switches;
		std::unordered_set<std::string> initial_checked;
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
		int program_duration;
		int compile_time_limit;
		int kill_wait;
		int output_limit_kill;
		int output_limit_warn;
		std::unordered_set<std::string> allow_file_exact;
		std::unordered_set<std::string> allow_file_prefix;
	};

	struct server_config {
		network_config network;
		jail_config jail;
		compiler_set compilers;
		std::unordered_map<std::string, switch_trait> switches;
	};

	server_config load_config(std::istream &is);
	std::string generate_displaying_compiler_config(const compiler_trait &compiler, const std::string &version, const std::unordered_map<std::string, switch_trait> &switches);
}

#endif
