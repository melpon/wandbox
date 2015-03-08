#ifndef LOAD_CONFIG_HPP_
#define LOAD_CONFIG_HPP_

#include <functional>
#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/optional.hpp>

namespace wandbox {
	namespace mendex = boost::multi_index;

	struct switch_trait {
		std::string group_name;
		std::string name;
		std::vector<std::string> flags;
		std::string display_name;
		boost::optional<std::string> display_flags;
		std::vector<std::string> conflicts;
		bool runtime;
		int insert_position;
	};

	typedef mendex::multi_index_container<
		switch_trait,
		mendex::indexed_by<
			mendex::sequenced<>,
			mendex::hashed_unique<mendex::member<switch_trait, std::string, &switch_trait::name>>,
			mendex::hashed_non_unique<mendex::member<switch_trait, std::string, &switch_trait::group_name>>
		>
	> local_switch_t;
	struct compiler_trait {
		std::string name;
		std::string language;
		std::vector<std::string> compile_command;
		std::vector<std::string> version_command;
		std::vector<std::string> run_command;
		std::string output_file;
		std::string display_name;
		std::string display_compile_command;
		std::string jail_name;
		std::vector<std::string> switches;
		local_switch_t local_switches;
		std::unordered_set<std::string> initial_checked;
		bool displayable;
		bool compiler_option_raw;
		bool runtime_option_raw;
	};
	typedef mendex::multi_index_container<compiler_trait, mendex::indexed_by<mendex::sequenced<>, mendex::hashed_unique<mendex::member<compiler_trait, std::string, &compiler_trait::name>>>> compiler_set;

	struct system_config {
		int listen_port;
		int max_connections;
		std::string basedir;
		std::string storedir;
	};

	struct jail_config {
		std::vector<std::string> jail_command;
		int program_duration;
		int compile_time_limit;
		int kill_wait;
		int output_limit_kill;
		int output_limit_warn;
	};

	struct server_config {
		system_config system;
		std::unordered_map<std::string, jail_config> jails;
		compiler_set compilers;
		std::unordered_map<std::string, switch_trait> switches;
	};

	server_config load_config(const std::vector<std::string> &cfgs);
	std::string generate_displaying_compiler_config(const compiler_trait &compiler, const std::string &version, const std::unordered_map<std::string, switch_trait> &switches);
}

#endif
