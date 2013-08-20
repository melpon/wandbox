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
#include <boost/optional.hpp>

namespace wandbox {
	namespace mendex = boost::multi_index;

	struct switch_trait {
		std::string name;
		std::vector<std::string> flags;
		std::string display_name;
		boost::optional<std::string> display_flags;
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
		std::vector<std::string> jail_command;
		std::string basedir;
		int program_duration;
		int compile_time_limit;
		int kill_wait;
		int output_limit_kill;
		int output_limit_warn;
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
