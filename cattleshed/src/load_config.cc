#include "load_config.hpp"

#include <map>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>

#include <boost/algorithm/string/join.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/variant.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/support_istream_iterator.hpp>
#include <boost/spirit/include/support_line_pos_iterator.hpp>
#include <boost/spirit/include/support_multi_pass.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/fusion/include/io.hpp>

#include "posixapi.hpp"

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
			top %= (obj | arr) > qi::eoi;
			val %= obj | arr | str | qi::int_ | qi::bool_;
			pair %= str > ':' > val;
			obj %= '{' > (((pair % ',') > -qi::lit(',')) | qi::eps) > '}';
			arr %= '[' > (((val % ',') > -qi::lit(',')) | qi::eps) > ']';
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
		}
	}

	compiler_set load_compiler_trait(const cfg::value &o) {
		using namespace detail;
		compiler_set ret;
		std::unordered_map<std::string, std::vector<std::string>> inherit_map;
		for (auto &x: boost::get<cfg::array>(boost::get<cfg::object>(o).at("compilers"))) {
			auto &y = boost::get<cfg::object>(x);
			compiler_trait t;
			t.name = get_str(y, "name");
			t.language = get_str(y, "language");
			t.compile_command = get_str_array(y, "compile-command");
			t.version_command = get_str_array(y, "version-command");
			t.run_command = get_str_array(y, "run-command");
			t.source_suffix = get_str(y, "source-suffix");
			t.display_name = get_str(y, "display-name");
			t.display_compile_command = get_str(y, "display-compile-command");
			t.jail_name = get_str(y, "jail-name");
			t.displayable = get_bool(y, "displayable");
			t.compiler_option_raw = get_bool(y, "compiler-option-raw");
			t.runtime_option_raw = get_bool(y, "runtime-option-raw");
			t.switches = get_str_array(y, "switches");
			for (auto &x: get_str_array(y, "initial-checked")) t.initial_checked.insert(std::move(x));
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
				if (sub.language.empty()) sub.language = x.language;
				if (sub.compile_command.empty()) sub.compile_command = x.compile_command;
				if (sub.version_command.empty()) sub.version_command = x.version_command;
				if (sub.run_command.empty()) sub.run_command = x.run_command;
				if (sub.source_suffix.empty()) sub.source_suffix = x.source_suffix;
				if (sub.display_name.empty()) sub.display_name = x.display_name;
				if (sub.display_compile_command.empty()) sub.display_compile_command = x.display_compile_command;
				if (sub.jail_name.empty()) sub.jail_name = x.jail_name;
				if (sub.switches.empty()) sub.switches = x.switches;
				ret.get<1>().replace(pos, sub);
			}
			inherit_map.erase(sub.name);
		}
		return ret;
	}

	system_config load_system_config(const cfg::value &values) {
		using namespace detail;
		const auto &o = boost::get<cfg::object>(boost::get<cfg::object>(values).at("system"));
		return { get_int(o, "listen-port"), get_int(o, "max-connections"), get_str(o, "basedir") };
	}

	 std::unordered_map<std::string, jail_config> load_jail_config(const cfg::value &values) {
		using namespace detail;
		std::unordered_map<std::string, jail_config> ret;
		for (const auto &p: boost::get<cfg::object>(boost::get<cfg::object>(values).at("jail"))) {
			 const auto &o = boost::get<cfg::object>(p.second);
			 jail_config x;
			 x.jail_command = get_str_array(o, "jail-command");
			 x.program_duration = get_int(o, "program-duration");
			 x.compile_time_limit = get_int(o, "compile-time-limit");
			 x.kill_wait = get_int(o, "kill-wait");
			 x.output_limit_kill = get_int(o, "output-limit-kill");
			 x.output_limit_warn = get_int(o, "output-limit-warn");
			 ret[p.first] = std::move(x);
		}
		return ret;
	}

	std::unordered_map<std::string, switch_trait> load_switches(const cfg::value &values) {
		using namespace detail;
		std::unordered_map<std::string, switch_trait> ret;
		for (const auto &a: boost::get<cfg::object>(boost::get<cfg::object>(values).at("switches"))) {
			const auto &s = boost::get<cfg::object>(a.second);
			switch_trait x;
			x.name = a.first;
			x.flags = get_str_array(s, "flags");
			x.display_name = get_str(s, "display-name");
			if (const auto v = find(s, "display-flags")) x.display_flags = boost::get<cfg::string>(*v);
			else x.display_flags = boost::none;
			x.conflicts = get_str_array(s, "conflicts");
			x.runtime = get_bool(s, "runtime");
			x.insert_position = get_int(s, "insert-position");
			ret[a.first] = std::move(x);
		}
		return ret;
	}

	struct read_fd_iterator {
		typedef std::input_iterator_tag iterator_category;
		typedef char value_type;
		typedef std::ptrdiff_t difference_type;
		typedef const char *pointer;
		typedef const char &reference;
		explicit read_fd_iterator(int fd = -1): buf(), off(), gone(), fd(fd) {
			fetch();
		}
		read_fd_iterator operator ++(int) {
			auto tmp = *this;
			++*this;
			return tmp;
		}
		read_fd_iterator &operator ++() {
			++off;
			++gone;
			fetch();
			return *this;
		}
		const char &operator *() const {
			return buf[off];
		}
		bool operator ==(const read_fd_iterator &o) const {
			return (fd == -1 && o.fd == -1) || (fd == o.fd && gone == o.gone);
		}
		bool operator !=(const read_fd_iterator &o) const {
			return !(*this == o);
		}
	private:
		void fetch() {
			if (fd == -1) return;
			if (off == buf.size()) {
				buf.resize(BUFSIZ);
				const auto r = ::read(fd, buf.data(), buf.size());
				if (r == 0) {
					fd = -1;
					off = 0;
					return;
				}
				if (r < 0) throw_system_error(errno);
				buf.resize(r);
				off = 0;
			}
		}
		std::vector<char> buf;
		std::size_t off;
		std::size_t gone;
		int fd;
	};

	template <typename Iter>
	struct parse_config_error: std::runtime_error {
		parse_config_error(const std::string &file, const boost::spirit::qi::expectation_failure<Iter> &e): std::runtime_error(make_what(file, e)) { }
		static std::string make_what(const std::string &file, const boost::spirit::qi::expectation_failure<Iter> &e) {
			const auto advance_for = [](Iter first, std::ptrdiff_t maxlen, Iter last) -> Iter {
				for (std::ptrdiff_t n = 0; n < maxlen && first != last; ++first, ++n) ;
				return first;
			};
			std::stringstream ss;
			ss << "parse error in file " << file << ":" << get_line(e.first)
				<< "\nwhile expecting " << e.what_
				<< "\nbut got " << std::string(e.first, advance_for(e.first, 128, e.last));
			return ss.str();
		}
	};

	cfg::value read_single_config_file(const std::shared_ptr<DIR> &at, const std::string &cfg) {
		namespace s = boost::spirit;
		namespace qi = boost::spirit::qi;
		typedef s::multi_pass<read_fd_iterator,
			s::iterator_policies::default_policy<
				s::iterator_policies::ref_counted,
				s::iterator_policies::no_check,
				s::iterator_policies::input_iterator,
				s::iterator_policies::split_std_deque>>
					functor_multi_pass_type;
		auto fd = unique_fd(::openat(dirfd_or_cwd(at), cfg.c_str(), O_RDONLY));
		if (fd.get() == -1) throw_system_error(errno);
		s::line_pos_iterator<functor_multi_pass_type> first(functor_multi_pass_type(read_fd_iterator(fd.get())));
		s::line_pos_iterator<functor_multi_pass_type> last;
		cfg::value o;
		try {
			qi::phrase_parse(first, last, cfg::config_grammar<decltype(first)>(), qi::space, o);
		} catch (qi::expectation_failure<decltype(first)> &e) {
			throw parse_config_error<decltype(first)>(cfg, e);
		}
		return o;
	}

	std::vector<cfg::value> read_config_file(const std::shared_ptr<DIR> &at, const std::string &name) {
		try {
			std::vector<cfg::value> ret;
			const auto dir = opendirat(at, name);
			std::vector<std::string> files;
			for (auto ent = readdir(dir.get()); ent; ent = readdir(dir.get())) {
				if (::strcmp(ent->d_name, ".") == 0 || ::strcmp(ent->d_name, "..") == 0) continue;
				files.emplace_back(ent->d_name);
			}
			std::sort(files.begin(), files.end());
			for (const auto &f: files) {
				const auto x = read_config_file(dir, f.c_str());
				ret.insert(ret.end(), x.begin(), x.end());
			}
			return ret;
		} catch (std::system_error &e) {
			if (e.code().value() != ENOTDIR) throw;
			return { read_single_config_file(at, name) };
		}
	}

	struct merge_cfgs_visitor {
		typedef cfg::value result_type;
		cfg::value operator ()(std::vector<cfg::value> a, const std::vector<cfg::value> &b) const {
			a.insert(a.end(), b.begin(), b.end());
			return a;
		}
		cfg::value operator ()(std::unordered_map<std::string, cfg::value> a, const std::unordered_map<std::string, cfg::value> &b) const {
			for (const auto &kv: b) {
				a[kv.first] = boost::apply_visitor(*this, a[kv.first], kv.second);
			}
			return a;
		}
		template <typename T, typename U>
		cfg::value operator ()(const T &, const U &b) const {
			return b;
		}
		template <typename T>
		cfg::value operator ()(const T &a, cfg::wandbox_cfg_tag) const {
			return a;
		}
		template <typename T>
		cfg::value operator ()(cfg::wandbox_cfg_tag, const T &b) const {
			return b;
		}
		cfg::value operator ()(cfg::wandbox_cfg_tag, cfg::wandbox_cfg_tag) const {
			return {};
		}
	};

	cfg::value merge_cfgs(const std::vector<cfg::value> &cfgs) {
		cfg::value ret;
		for (const auto &c: cfgs) {
			ret = boost::apply_visitor(merge_cfgs_visitor(), ret, c);
		}
		return ret;
	}

	server_config load_config(const std::vector<std::string> &cfgs) {
		std::vector<cfg::value> os;
		for (const auto &c: cfgs) {
			const auto x = read_config_file(nullptr, c);
			os.insert(os.end(), x.begin(), x.end());
		}
		const auto o = merge_cfgs(os);
		return { load_system_config(o), load_jail_config(o), load_compiler_trait(o), load_switches(o) };
	}

	template <typename Iter>
	struct char_escaping_iterator: std::iterator<std::input_iterator_tag, typename std::iterator_traits<Iter>::value_type> {
		typedef std::input_iterator_tag iterator_category;
		typedef typename std::iterator_traits<Iter>::value_type value_type;
		typedef typename std::iterator_traits<Iter>::difference_type difference_type;
		typedef const value_type &reference;
		typedef const value_type *pointer;
		char_escaping_iterator(Iter base): base_(base), escaping(false) { }
		Iter base() const { return base_; }
		reference operator *() const {
			if (escaping) {
				switch (*base_) {
				case '\\': return escape_backslash;
				case '\"': return escape_dquot;
				case '\t': return escape_tab;
				case '\r': return escape_cr;
				case '\n': return escape_nl;
				}
			} else {
				switch (*base_) {
				case '\\':
				case '\"':
				case '\t':
				case '\r':
				case '\n':
					return escape_backslash;
				}
			}
			return *base_;
		}
		char_escaping_iterator &operator ++() {
			if (escaping) {
				++base_;
				escaping = false;
			} else {
				switch (*base_) {
				case '\\':
				case '\"':
				case '\t':
				case '\r':
				case '\n':
					escaping = true;
					break;
				default:
					++base_;
				}
			}
			return *this;
		}
		char_escaping_iterator operator ++(int) {
			auto tmp(*this);
			++*this;
			return tmp;
		}
		bool operator ==(const char_escaping_iterator &rhs) const {
			return base_ == rhs.base_ && escaping == rhs.escaping;
		}
		bool operator !=(const char_escaping_iterator &rhs) const {
			return !(*this == rhs);
		}
		typename std::enable_if<std::is_convertible<decltype(std::declval<Iter>() < std::declval<Iter>()), bool>::value, bool>::type operator < (const char_escaping_iterator &rhs) const {
			return (base_ < rhs.base_) || (base_ == rhs.base_ && !escaping && rhs.escaping);
		}
		typename std::enable_if<std::is_convertible<decltype(std::declval<Iter>() < std::declval<Iter>()), bool>::value, bool>::type operator > (const char_escaping_iterator &rhs) const {
			return rhs < *this;
		}
		typename std::enable_if<std::is_convertible<decltype(std::declval<Iter>() < std::declval<Iter>()), bool>::value, bool>::type operator <=(const char_escaping_iterator &rhs) const {
			return *this < rhs || *this == rhs;
		}
		typename std::enable_if<std::is_convertible<decltype(std::declval<Iter>() < std::declval<Iter>()), bool>::value, bool>::type operator >=(const char_escaping_iterator &rhs) const {
			return rhs < *this || rhs == *this;
		}
	private:
		Iter base_;
		bool escaping;
		static const char escape_backslash;
		static const char escape_dquot;
		static const char escape_tab;
		static const char escape_cr;
		static const char escape_nl;
	};
	template <typename Iter>
	const char char_escaping_iterator<Iter>::escape_backslash = '\\';
	template <typename Iter>
	const char char_escaping_iterator<Iter>::escape_dquot = '\"';
	template <typename Iter>
	const char char_escaping_iterator<Iter>::escape_tab = 't';
	template <typename Iter>
	const char char_escaping_iterator<Iter>::escape_cr = 'r';
	template <typename Iter>
	const char char_escaping_iterator<Iter>::escape_nl = 'n';
	template <typename Iter>
	char_escaping_iterator<Iter> char_escaper(Iter ite) {
		return { ite };
	}
	std::string json_stringize(const std::string &in) {
		return std::string(char_escaper(in.begin()), char_escaper(in.end()));
	}

	std::string generate_displaying_compiler_config(const compiler_trait &compiler, const std::string &version, const std::unordered_map<std::string, switch_trait> &switches) {
		std::vector<std::string> swlist;
		{
			std::unordered_set<std::string> used;
			for (const auto &swname: compiler.switches) {
				if (used.count(swname) != 0) continue;
				const auto ite = switches.find(swname);
				if (ite == switches.end()) continue;
				const auto &sw = ite->second;
				if (sw.conflicts.empty()) {
					used.insert(swname);
					swlist.emplace_back(
						"{"
							"\"name\":\"" + json_stringize(sw.name) + "\","
							"\"type\":\"single\","
							"\"display-name\":\"" + json_stringize(sw.display_name) + "\","
							"\"display-flags\":\"" + json_stringize((sw.display_flags ? *sw.display_flags : boost::algorithm::join(sw.flags, " "))) + "\","
							"\"default\":" + (compiler.initial_checked.count(sw.name) != 0 ? "true" : "false") +
						"}");
				} else {
					std::function<void(const std::string &)> f;
					std::unordered_set<std::string> set;
					f = [&](const std::string &swname) {
						const auto ite = switches.find(swname);
						if (ite == switches.end()) return;
						const auto &sw = ite->second;
						for (const auto &c: sw.conflicts) {
							const auto ite = switches.find(c);
							if (ite == switches.end()) continue;
							if (set.insert(c).second) f(c);
						}
					};
					f(swname);
					std::vector<std::string> sel;
					std::string def = swname;
					for (const auto &swname: compiler.switches) {
						const auto &sw = switches.at(swname);
						if (set.count(swname) == 0) continue;
						sel.emplace_back(
							"{"
								"\"name\":\"" + json_stringize(sw.name) + "\","
								"\"display-name\":\"" + json_stringize(sw.display_name) + "\","
								"\"display-flags\":\"" + json_stringize((sw.display_flags ? *sw.display_flags : boost::algorithm::join(sw.flags, " "))) + "\""
							"}");
						if (compiler.initial_checked.count(sw.name) != 0) def = swname;
					}
					swlist.emplace_back(
						"{"
							"\"type\":\"select\","
							"\"default\":\"" + json_stringize(def) + "\","
							"\"options\":[" + boost::algorithm::join(sel, ",") + "]"
						"}");
					used.insert(set.begin(), set.end());
				}
			}
		}
		return
			"{"
				"\"name\":\"" + json_stringize(compiler.name) + "\","
				"\"language\":\"" + json_stringize(compiler.language) + "\","
				"\"display-name\":\"" + json_stringize(compiler.display_name) + "\","
				"\"version\":\"" + json_stringize(version) + "\","
				"\"display-compile-command\":\"" + json_stringize(compiler.display_compile_command) + "\","
				"\"compiler-option-raw\":" + (compiler.compiler_option_raw ? "true" : "false") + ","
				"\"runtime-option-raw\":" + (compiler.runtime_option_raw ? "true" : "false") + ","
				"\"switches\":[" + boost::algorithm::join(swlist, ",") + "]"
			"}";
	}
}


