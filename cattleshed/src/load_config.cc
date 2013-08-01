#include "load_config.hpp"

#include <map>
#include <string>
#include <vector>
#include <unordered_map>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/variant.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/fusion/include/io.hpp>
#include <boost/io/ios_state.hpp>

namespace wandbox {
namespace cfg {
	struct wandbox_cfg_tag {};
	typedef boost::make_recursive_variant<
		wandbox_cfg_tag,
		std::string,
		std::vector<boost::recursive_variant_>,
		std::unordered_map<std::string, boost::recursive_variant_>,
		int,
		bool
	>::type value;
	typedef std::string string;
	typedef std::unordered_map<string, value> object;
	typedef std::vector<value> array;

	namespace qi = boost::spirit::qi;

	template <typename Iter>
	struct config_grammar: qi::grammar<Iter, value(), qi::space_type> {
		config_grammar(): qi::grammar<Iter, value(), qi::space_type>(top) {
			namespace phx = boost::phoenix;
			top %= obj | arr;
			val %= obj | arr | str | qi::int_ | qi::bool_;
			pair %= str > ':' > val;
			obj %= '{' > ((pair % ',') > -qi::lit(',')) > '}';
			arr %= '[' > ((val % ',') > -qi::lit(',')) > ']';
			str %= qi::lexeme['\"' > *(('\\' > qi::char_("\\\"\'\t\r\n")) | (qi::char_-'\"')) > '\"'];
			//debug(top);
			//debug(val);
			//debug(pair);
			//debug(obj);
			//debug(arr);
			//debug(str);
		}
		qi::rule<Iter, value(), qi::space_type> top;
		qi::rule<Iter, std::pair<string, value>(), qi::space_type> pair;
		qi::rule<Iter, object(), qi::space_type> obj;
		qi::rule<Iter, array(), qi::space_type> arr;
		qi::rule<Iter, value(), qi::space_type> val;
		qi::rule<Iter, string(), qi::space_type> str;
	};

	struct operator_output: boost::static_visitor<std::ostream &> {
		explicit operator_output(std::ostream &os, int indent): os(os), indent(indent) { }
		std::ostream &operator ()(const string &str) const {
			return os << '\"' << str << '\"';
		}
		std::ostream &operator ()(const object &obj) const {
			if (obj.empty()) return os << "{}";
			os << "{\n";
			const operator_output print(os, indent+1);
			for (const auto &x: obj) {
				put_indent(1);
				print(x.first) << ':';
				boost::apply_visitor(print, x.second) << ",\n";
			}
			put_indent(0);
			os << "}";
			return os;
		}
		std::ostream &operator ()(const array &arr) const {
			if (arr.empty()) return os << "[]";
			os << "[\n";
			const operator_output print(os, indent+1);
			for (const auto &x: arr) {
				put_indent(1);
				boost::apply_visitor(print, x) << ",\n";
			}
			put_indent(0);
			os << "]";
			return os;
		}
		std::ostream &operator ()(const int &i) const {
			return os << i;
		}
		std::ostream &operator ()(const bool& bool_) const {
			return os << (bool_ ? "true" : "false");
		}
		std::ostream &operator ()(const wandbox_cfg_tag &) const { return os; }
		void put_indent(int add) const {
			os << std::string(indent+add, ' ');
		}
		std::ostream &os;
		int indent;
	};

	inline std::ostream &operator <<(std::ostream &os, const value &val) {
		return boost::apply_visitor(operator_output(os, 0), val);
	}
}

	namespace detail {
		template <typename Map>
		boost::optional<const typename Map::mapped_type &> find(const Map &m, const typename Map::key_type &k) {
			const auto ite = m.find(k);
			if (ite == m.end()) return {};
			return ite->second;
		}
		inline std::string get_str(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) return boost::get<cfg::string>(*v);
			return {};
		};
		inline int get_int(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) return boost::get<int>(*v);
			return 0;
		}
		inline bool get_bool(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) return boost::get<bool>(*v);
			return false;
		}
		inline std::vector<cfg::string> get_str_array(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) {
				if (const auto *s = boost::get<cfg::string>(&*v)) return { *s };
				std::vector<cfg::string> ret;
				for (const auto &s: boost::get<cfg::array>(*v)) ret.emplace_back(boost::get<cfg::string>(s));
				return ret;
			}
			return {};
		};
		inline switch_set get_switches(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) {
				switch_set ret;
				for (const auto &a: boost::get<cfg::array>(*v)) {
					const auto &s = boost::get<cfg::object>(a);
					switch_trait x;
					x.name = get_str(s, "name");
					x.flags = get_str_array(s, "flags");
					x.default_ = get_bool(s, "default");
					x.display_name = get_str(s, "display-name");
					ret.push_back(x);
				}
				return ret;
			}
			return {};
		};
	}

	compiler_set load_compiler_trait(const cfg::value &o) {
		using namespace detail;
		compiler_set ret;
		std::unordered_map<std::string, std::vector<std::string>> inherit_map;
		for (auto &x: boost::get<cfg::array>(boost::get<cfg::object>(o).at("compilers"))) {
			auto &y = boost::get<cfg::object>(x);
			compiler_trait t;
			t.name = get_str(y, "name");
			t.output_file = get_str(y, "output-file");
			t.language = get_str(y, "language");
			t.compile_command = get_str_array(y, "compile-command");
			t.version_command = get_str_array(y, "version-command");
			t.run_command = get_str_array(y, "run-command");
			t.source_suffix = get_str(y, "source-suffix");
			t.display_name = get_str(y, "display-name");
			t.display_compile_command = get_str(y, "display-compile-command");
			t.displayable = get_bool(y, "displayable");
			t.switches = get_switches(y, "switches");
			const auto inherits = get_str_array(y, "inherits");
			if (!inherits.empty()) inherit_map[t.name] = inherits;
			ret.push_back(t);
		}
		while (!inherit_map.empty()) {
			const auto ite = std::find_if(inherit_map.begin(), inherit_map.end(), [&](const std::pair<const std::string, std::vector<std::string>> &p) {
				return std::all_of(p.second.begin(), p.second.end(), [&](const std::string &target) {
					return inherit_map.find(target) == inherit_map.end();
				});
			});
			if (ite == inherit_map.end()) break;
			const auto pos = ret.get<1>().find(ite->first);
			auto sub = *pos;
			for (const auto &target: ite->second) {
				const auto &x = *ret.get<1>().find(target);
				if (sub.output_file.empty()) sub.output_file = x.output_file;
				if (sub.language.empty()) sub.language = x.language;
				if (sub.compile_command.empty()) sub.compile_command = x.compile_command;
				if (sub.version_command.empty()) sub.version_command = x.version_command;
				if (sub.run_command.empty()) sub.run_command = x.run_command;
				if (sub.source_suffix.empty()) sub.source_suffix = x.source_suffix;
				if (sub.display_name.empty()) sub.display_name = x.display_name;
				if (sub.display_compile_command.empty()) sub.display_compile_command = x.display_compile_command;
				if (sub.switches.empty()) sub.switches = x.switches;
				ret.get<1>().replace(pos, sub);
			}
			inherit_map.erase(sub.name);
		}
		return ret;
	}

	network_config load_network_config(const cfg::value &values) {
		using namespace detail;
		const auto &o = boost::get<cfg::object>(boost::get<cfg::object>(values).at("network"));
		return { get_int(o, "listen-port") };
	}

	jail_config load_jail_config(const cfg::value &values) {
		using namespace detail;
		const auto &o = boost::get<cfg::object>(boost::get<cfg::object>(values).at("jail"));
		jail_config x;
		x.exe = get_str(o, "jail");
		x.basedir = get_str(o, "basedir");
		x.max_address_space = get_int(o, "max-address-space");
		x.max_cpu_time = get_int(o, "max-cpu-time");
		x.max_data_segment = get_int(o, "max-data-segment");
		x.max_file_size = get_int(o, "max-file-size");
		x.max_open_file = get_int(o, "max-open-file");
		x.nice = get_int(o, "nice");
		x.program_duration = get_int(o, "program-duration");
		x.compile_time_limit = get_int(o, "compile-time-limit");
		x.kill_wait = get_int(o, "kill-wait");
		x.output_limit_kill = get_int(o, "output-limit-kill");
		x.output_limit_warn = get_int(o, "output-limit-warn");
		for (const auto &y: get_str_array(o, "allow-file-exact")) {
			x.allow_file_exact.insert(y);
		}
		for (const auto &y: get_str_array(o, "allow-file-prefix")) {
			x.allow_file_prefix.insert(y);
		}
		return x;
	}

	server_config load_config(std::istream &is) {
		boost::io::ios_flags_saver sv(is);
		is.unsetf(std::ios::skipws);
		cfg::value o;
		namespace s = boost::spirit;
		namespace qi = boost::spirit::qi;
		auto first = (s::istream_iterator(is));
		const auto last = decltype(first)();
		qi::phrase_parse(first, last, cfg::config_grammar<decltype(first)>(), qi::space, o);
		return { load_network_config(o), load_jail_config(o), load_compiler_trait(o) };
	}
}
