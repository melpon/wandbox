#include <string>
#include <vector>
#include <unordered_map>

#include <boost/variant.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/fusion/include/io.hpp>

namespace wandbox {
namespace cfg {
	struct wandbox_cfg_tag {};
	typedef boost::make_recursive_variant<
		wandbox_cfg_tag,
		std::string,
		std::vector<boost::recursive_variant_>,
		std::unordered_map<std::string, boost::recursive_variant_>
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
			val %= obj | arr | str;
			pair %= str >> ':' >> val;
			obj %= '{' > ((pair % ',') > -qi::lit(',')) > '}';
			arr %= '[' > ((val % ',') > -qi::lit(',')) > ']';
			str %= qi::lexeme['\"' > *(qi::char_-'\"') > '\"'];
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

	struct compiler_trait {
		std::string name;
		std::string output_file;
		std::string language;
		std::vector<std::string> compile_command;
		std::vector<std::string> version_command;
		std::vector<std::string> run_command;
		std::map<std::string, std::vector<std::string>> switches;
		std::string source_suffix;
		std::string display_name;
	};

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
		inline std::vector<cfg::string> get_str_array(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) {
				if (const auto *s = boost::get<cfg::string>(&*v)) return { *s };
				std::vector<cfg::string> ret;
				for (const auto &s: boost::get<cfg::array>(*v)) ret.emplace_back(boost::get<cfg::string>(s));
				return ret;
			}
			return {};
		};
		inline std::map<cfg::string, std::vector<cfg::string>> get_str_map(const cfg::object &x, const cfg::string &key) {
			if (const auto &v = find(x, key)) {
				std::map<cfg::string, std::vector<cfg::string>> ret;
				for (const auto &p: boost::get<cfg::object>(*v)) {
					auto &r = ret[p.first];
					if (const auto *s = boost::get<cfg::string>(&p.second)) {
						r.emplace_back(*s);
					} else {
						for (const auto &s: boost::get<cfg::array>(p.second)) r.emplace_back(boost::get<cfg::string>(s));
					}
				}
				return ret;
			}
			return {};
		};
	}
	template <typename Iter>
	std::map<std::string, compiler_trait> load_compiler_trait(Iter begin, Iter end) {
		using namespace detail;
		cfg::value o;
		qi::phrase_parse(begin, end, cfg::config_grammar<Iter>(), qi::space, o);
		std::map<std::string, compiler_trait> ret;
		std::map<std::string, std::vector<std::string>> inherit_map;
		for (auto &x: boost::get<cfg::array>(o)) {
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
			t.switches = get_str_map(y, "switches");
			const auto inherits = get_str_array(y, "inherits");
			if (!inherits.empty()) inherit_map[t.name] = inherits;
			ret[t.name] = t;
		}
		while (!inherit_map.empty()) {
			const auto ite = std::find_if(inherit_map.begin(), inherit_map.end(), [&](const std::pair<const std::string, std::vector<std::string>> &p) {
				return std::all_of(p.second.begin(), p.second.end(), [&](const std::string &target) {
					return inherit_map.find(target) == inherit_map.end();
				});
			});
			if (ite == inherit_map.end()) break;
			auto &sub = ret[ite->first];
			for (const auto &target: ite->second) {
				const auto &x = ret[target];
				if (sub.output_file.empty()) sub.output_file = x.output_file;
				if (sub.language.empty()) sub.language = x.language;
				if (sub.compile_command.empty()) sub.compile_command = x.compile_command;
				if (sub.version_command.empty()) sub.version_command = x.version_command;
				if (sub.run_command.empty()) sub.run_command = x.run_command;
				if (sub.source_suffix.empty()) sub.source_suffix = x.source_suffix;
				if (sub.display_name.empty()) sub.display_name = x.display_name;
				for (const auto &s: x.switches) {
					if (sub.switches[s.first].empty()) sub.switches[s.first] = s.second;
				}
			}
			inherit_map.erase(sub.name);
		}
		return ret;
	}

}
