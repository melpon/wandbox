//#define BOOST_ERROR_CODE_HEADER_ONLY

#include "common.hpp"
#include "load_config.hpp"
#include <map>
#include <string>
#include <vector>
#include <array>
#include <thread>
#include <sstream>
#include <set>
#include <functional>
#include <list>
#include <fstream>
#include <system_error>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/istream_range.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/join.hpp>
#include <boost/program_options.hpp>
#include <boost/system/system_error.hpp>

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <libgen.h>

namespace wandbox {

	namespace brange = boost::range;

	using std::shared_ptr;
	using std::unique_ptr;
	using std::make_shared;
	using std::string;
	using std::vector;
	using std::array;
	using std::list;
	using std::size_t;
	using std::move;
	using std::ref;
	using std::cref;
	using boost::system::error_code;
	using std::placeholders::_1;
	using std::placeholders::_2;
	using std::placeholders::_3;

	struct ignore_param {
		template <typename ...Args>
		ignore_param(Args &&...) {}
	};

	template <typename T>
	inline boost::iterator_range<T *> make_iterator_range_from_memory(T *head, size_t size) {
		return boost::make_iterator_range(head, head + size);
	}
	template <typename T>
	inline boost::iterator_range<const T *> make_iterator_range_from_memory(const T *head, size_t size) {
		return boost::make_iterator_range(head, head + size);
	}
	template <typename Ptr>
	inline boost::iterator_range<Ptr> make_iterator_range_from_buffer(const asio::const_buffer &b) {
		return make_iterator_range_from_memory(asio::buffer_cast<Ptr>(b), asio::buffer_size(b));
	}
	struct lazy_range_search_t {
		template <typename R, typename S>
		struct result { typedef  typename boost::range_iterator<const R>::type type; };
		template <typename R, typename S>
		typename result<R, S>::type operator ()(const R &r, const S &s) const {
			return std::search(boost::begin(r), boost::end(r), boost::begin(s), boost::end(s));
		}
	};

	template <typename M>
	void print_map(const M &m) {
		std::for_each(m.begin(), m.end(), [](const typename M::value_type &p) {
			std::cout << p.first << ':' << p.second << '\n';
		});
		std::cout << std::flush;
	}

#undef _P_WAIT
#undef _P_NOWAIT
#undef _P_OVERLAY
#undef _P_NOWAITO
#undef _P_DETACH
	constexpr int _P_WAIT = 0;
	constexpr int _P_NOWAIT = 1;
	constexpr int _P_OVERLAY = 2;
	constexpr int _P_NOWAITO = 3;
	constexpr int _P_DETACH = 4;

	struct child_process {
		intptr_t pid;
		int fd_stdin;
		int fd_stdout;
		int fd_stderr;
	};
	struct unique_fd {
		unique_fd(int fd): fd(fd) { }
		~unique_fd() { close(); }
		unique_fd(const unique_fd &) = delete;
		unique_fd(unique_fd &&o): fd(-1) {
			std::swap(fd, o.fd);
		}
		unique_fd &operator =(const unique_fd &) = delete;
		unique_fd &operator =(unique_fd &&o) {
			std::swap(fd, o.fd);
			if (fd != o.fd) o.close();
			return *this;
		}
		int get() const { return fd; }
		explicit operator bool() const { return fd != 1; }
		bool operator !() const { return fd == -1; }
	private:
		void close() {
			if (*this) ::close(fd);
			fd = -1;
		}
		int fd;
	};

	inline std::ostream &print(std::ostream &os) { return os; }
	template <typename A, typename ...Args>
	std::ostream &print(std::ostream &os, A &&a, Args &&...args) {
		return print(os << std::forward<A>(a) << ", ", std::forward<Args>(args)...);
	}

	child_process piped_spawn(int mode, DIR *workdir, const vector<string> &argv) {
		vector<vector<char>> x;
		for (const auto &s: argv) x.emplace_back(s.c_str(), s.c_str()+s.length()+1);
		vector<char *> a;
		for (auto &s: x) a.emplace_back(s.data());
		a.push_back(nullptr);
		int pipe_stdin[2] = { -1, -1 }, pipe_stdout[2] = { -1, -1 }, pipe_stderr[2] = { -1, -1 };
		if (mode == _P_OVERLAY) {
			if (::fchdir(::dirfd(workdir)) == -1) goto end;;
			std::cout << "exec error : " << ::execv(x.front().data(), a.data()) << ',';
			std::cout << strerror(errno) << std::endl;
			exit(-1);
		}
		if (::pipe(pipe_stdin) == -1) goto end;
		if (::pipe(pipe_stdout) == -1) goto end;
		if (::pipe(pipe_stderr) == -1) goto end;
		{
			const pid_t pid = ::fork();
			if (pid == 0) {
				if (::fchdir(::dirfd(workdir)) == -1) exit(-1);
				::dup2(pipe_stdin[0], 0);
				::dup2(pipe_stdout[1], 1);
				::dup2(pipe_stderr[1], 2);
				::close(pipe_stdin[0]);
				::close(pipe_stdin[1]);
				::close(pipe_stdout[0]);
				::close(pipe_stdout[1]);
				::close(pipe_stderr[0]);
				::close(pipe_stderr[1]);
				::fcntl(0, F_SETFD, (long)0);
				::fcntl(1, F_SETFD, (long)0);
				::fcntl(2, F_SETFD, (long)0);
				std::cout << "exec error : " << ::execv(x.front().data(), a.data()) << ',';
				std::cout << strerror(errno) << std::endl;
				exit(-1);
			} else if (pid > 0) {
				::close(pipe_stdin[0]);
				::close(pipe_stdout[1]);
				::close(pipe_stderr[1]);

				child_process child { pid, pipe_stdin[1], pipe_stdout[0], pipe_stderr[0] };
				if (mode == _P_WAIT) {
					int st;
					::waitpid(pid, &st, 0);
					child.pid = st;
					return child;
				}
				return child;
			}
		}
	end:
		const auto close = [](int fd) { if (fd != -1) ::close(fd); };
		close(pipe_stdin[0]); close(pipe_stdin[1]);
		close(pipe_stdout[0]); close(pipe_stderr[1]);
		close(pipe_stdout[0]); close(pipe_stderr[1]);
		return child_process { -1, -1, -1, -1 };
	}

	struct end_read_condition {
		explicit end_read_condition(std::size_t min = 0): min(min) { }
		template <typename Iter>
		std::pair<Iter, bool> operator ()(Iter first, Iter last) const {
			const auto d = std::distance(first, last);
			if (d < min) return { first, false };
			if (d >= BUFSIZ) return { last, true };
			const auto ite = std::find(first, last, '\n');
			return { ite, ite != last };
		}
		std::size_t min;
	};
	__attribute__((noreturn)) void throw_system_error(int err) {
		throw std::system_error(err, std::system_category());
	}
	string dirname(const string &path) {
		std::vector<char> buf(path.begin(), path.end());
		buf.push_back(0);
		return ::dirname(buf.data());
	}
	string basename(const string &path) {
		std::vector<char> buf(path.begin(), path.end());
		buf.push_back(0);
		return ::basename(buf.data());
	}
	string readlink(const string &path) {
		std::vector<char> buf(PATH_MAX);
		while (true) {
			const int ret = ::readlink(path.c_str(), buf.data(), buf.size());
			if (ret < 0) throw_system_error(errno);
			if (ret < buf.size()) return { buf.begin(), buf.begin()+ret };
			buf.resize(buf.size()*2);
		}
	}
	string realpath(const string &path) {
		const std::shared_ptr<char> p(::realpath(path.c_str(), nullptr), &::free);
		if (!p) throw_system_error(errno);
		return p.get();
	}

	void mkdir(const string &path, ::mode_t mode) {
		if (::mkdir(path.c_str(), mode) < 0) throw_system_error(errno);
	}

	std::shared_ptr<DIR> opendir(const string &path) {
		DIR *dir = ::opendir(path.c_str());
		if (!dir) throw_system_error(errno);
		return std::shared_ptr<DIR>(dir, &::closedir);
	}

	string mkdtemp(const string &base) {
		std::vector<char> buf(base.begin(), base.end());
		buf.push_back(0);
		if (!::mkdtemp(buf.data())) throw_system_error(errno);
		return buf.data();
	}
}

namespace boost {
	namespace asio {
		template <>
		struct is_match_condition<wandbox::end_read_condition>: boost::mpl::true_ { };
	}
}

namespace wandbox {

	string ptracer;
	string config_file;
	server_config config;

	struct thread_compare {
		bool operator ()(const std::thread &x, const std::thread &y) const {
			return x.get_id() < y.get_id();
		}
	};

	struct compiler_bridge {
		bool is_any_of(const string& str, const string& value) const {
			vector<string> result;
			boost::algorithm::split(result, str, [](char c) { return c == ','; });
			return std::any_of(result.begin(), result.end(), [&](const string& v) { return v == value; });
		}
		const compiler_trait &get_compiler() const {
			const auto compiler = [this]() -> string {
				string ret;
				const auto &s = received.at("Control");
				auto ite = s.begin();
				qi::parse(ite, s.end(), "compiler=" >> *qi::char_, ret);
				return ret;
			}();
		    return *config.compilers.get<1>().find(compiler);
		}
		vector<string> get_compiler_arg() const {
			const auto &c = get_compiler();
			vector<string> args = c.compile_command;
			const auto it = received.find("CompilerOption");
			if (it != received.end()) {
				for (const auto &sw: c.switches) {
					if (is_any_of(it->second, sw.name)) {
						args.insert(args.end(), sw.flags.begin(), sw.flags.end());
					}
				}
			}
			return args;
		}
		void send_version() {
			const auto proc = [this](const vector<string>& args) -> std::string {
				const auto workdir = opendir("/");
				const child_process c = piped_spawn(_P_WAIT, workdir.get(), args);
				::close(c.fd_stdin);
				::close(c.fd_stderr);
				stream_descriptor_pair s(aio, c.fd_stdout);
				if (c.pid == -1 || !WIFEXITED(c.pid) || WEXITSTATUS(c.pid) != 0) return "";
				error_code ec;
				asio::read(s.stream, s.buf, ec);
				return std::string(asio::buffer_cast<const char *>(s.buf.data()), asio::buffer_size(s.buf.data()));
			};
			string line;
			for (const auto &c: config.compilers) {
				if (!c.displayable) continue;
				if (c.version_command.empty()) continue;
				string ver;
				{
					std::stringstream ss(proc(c.version_command));
					getline(ss, ver);
				}
				if (ver.empty()) continue;
				// NOTE: Each variables must not contain <LF> or <COMMA> or <TAB>.
				// <line> ::= name,language,display_name,ver,display_compile_command<switches><LF>
				// <switches> ::= (,name<TAB>flags<TAB>default<TAB>display_name)*
				line += c.name + "," + c.language + "," + c.display_name + "," + ver + "," + c.display_compile_command;
				for (const auto &sw: c.switches) {
					line +=
						"," + sw.name +
						"\t" + boost::algorithm::join(sw.flags, " ") +
						"\t" + (sw.default_ ? "true" : "false") +
						"\t" + sw.display_name;
				}
				line += "\n";
			}
			line = encode_qp(line);
			const auto str = ([&]() -> string {
				std::stringstream ss;
				ss << "VersionResult " << line.length() << ':' << line << '\n';
				return ss.str();
			})();
			aio.post([this, str] {
				error_code ec;
				asio::write(sock, asio::buffer(str), ec);
			});
			aio.run();
		}
		template <typename Stream>
		struct stream_pair {
			template <typename ...Args>
			stream_pair(asio::io_service &aio, Args &&...args): stream(aio, std::forward<Args>(args)...), buf() { }
			Stream stream;
			asio::streambuf buf;
		};
		typedef stream_pair<asio::posix::stream_descriptor> stream_descriptor_pair;

		bool wait_run_command() {
			asio::streambuf buf;
			bool exhausted = false;
			while (true) {
				asio::read_until(sock, buf, end_read_condition(asio::buffer_size(buf.data()) + (exhausted ? 1 : 0)));
				const auto begin = asio::buffer_cast<const char *>(buf.data());
				auto ite = begin;
				const auto end = ite + asio::buffer_size(buf.data());

				std::string command;
				int len = 0;
				std::string data;
				std::cout << std::string(ite, end) << std::endl;
				if (qi::parse(ite, end, +(qi::char_ - qi::space) >> qi::omit[*qi::space] >> qi::omit[qi::int_[phx::ref(len) = qi::_1]] >> qi::omit[':'] >> qi::repeat(phx::ref(len))[qi::char_] >> qi::omit[qi::eol], command, data)) {

					std::cout << "command: " << command << " : " << data << std::endl;
					if (command == "Control" && data == "run") return true;
					if (command == "Version") {
						send_version();
						return false;
					}
					received[command] += decode_qp(data);
					exhausted = false;
				} else {
					exhausted = true;
				}
				buf.consume(ite - begin);
			}
		}

		void operator ()() {
			// io thread

			if (!wait_run_command()) return;
			const string srcname = "prog" + get_compiler().source_suffix;

			auto &s = received["Source"];
			{
				int fd = ::openat(::dirfd(workdir.get()), srcname.c_str(), O_WRONLY|O_CLOEXEC|O_CREAT|O_TRUNC, 0600);
				for (std::size_t offset = 0; offset < s.length(); ) {
					const auto wrote = ::write(fd, s.data()+offset, s.length()-offset);
					if (wrote < 0) throw std::runtime_error("write(2) failed");
					offset += wrote;
				}
				::fsync(fd);
				::close(fd);
			}
			std::cout << "Source <<EOF\n" << s << "\nEOF" << std::endl;

			const child_process c = piped_spawn(_P_NOWAIT, workdir.get(), get_compiler_arg());
			std::cout << "gcc : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
			::close(c.fd_stdin);
			cc_pid = c.pid;

			aio.post([this] {
				error_code ec;
				const string str = "Control 5:Start\n";
				asio::write(sock, asio::buffer(str), ec);
			});

			pipes.emplace_front(ref(aio), c.fd_stdout);
			auto cc_stdout = pipes.begin();
			asio::async_read_until(
				cc_stdout->stream, cc_stdout->buf, end_read_condition(),
				std::bind<void>(
					ref(*this),
					cc_stdout,
					"CompilerMessageS",
					_1,
					_2));

			pipes.emplace_front(ref(aio), c.fd_stderr);
			auto cc_stderr = pipes.begin();
			asio::async_read_until(
				cc_stderr->stream, cc_stderr->buf, end_read_condition(),
				std::bind<void>(
					ref(*this),
					cc_stderr,
					"CompilerMessageE",
					_1,
					_2));

			aio.run();
		}

		void send_exitcode(int code) {
			aio.post([this,code] {
				error_code ec;
				string str;
				if (WIFSIGNALED(code)) {
					const char *sig = ::strsignal(WTERMSIG(code));
					if (not sig) sig = "";
					str += "Signal " + boost::lexical_cast<std::string>(::strlen(sig)) + ":" + sig + "\n";
				}
				if (WIFEXITED(code)) {
					const auto c = boost::lexical_cast<std::string>(WEXITSTATUS(code));
					str += "ExitCode " + boost::lexical_cast<std::string>(c.length()) + ":" + c + "\n";
				}
				str += "Control 6:Finish\n";
				asio::write(sock, asio::buffer(str), ec);
				sock.close();
				std::cout << "closed" << std::endl;
				aio.post(std::bind<void>(&asio::io_service::stop, ref(aio)));
			});
		}

		void check_finish() {
			if (!pipes.empty()) return;
			if (prog_pid) {
				int st;
				::waitpid(prog_pid, &st, 0);
				std::cout << "Program finished: code=" << st << std::endl;
				send_exitcode(st);
			} else {
				int st;
				::waitpid(cc_pid, &st, 0);
				std::cout << "Compile finished: code=" << st << std::endl;
				if (st) {
					prog_pid = -1;
					std::cout << "COMPILE ERROR" << std::endl;
					send_exitcode(st);
				} else {
					vector<string> runargs = get_compiler().run_command;
					runargs.insert(runargs.begin(), { ptracer, "--config=" + config_file, "--" });
					child_process c = piped_spawn(_P_NOWAIT, workdir.get(), runargs);
					std::cout << "a.out : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
					::close(c.fd_stdin);
					prog_pid = c.pid;

					pipes.emplace_front(ref(aio), c.fd_stdout);
					auto aout_stdout = pipes.begin();
					asio::async_read_until(
						aout_stdout->stream, aout_stdout->buf, end_read_condition(),
						std::bind<void>(
							ref(*this),
							aout_stdout,
							"StdOut",
							_1,
							_2));

					pipes.emplace_front(ref(aio), c.fd_stderr);
					auto aout_stderr = pipes.begin();
					asio::async_read_until(
						aout_stderr->stream, aout_stderr->buf, end_read_condition(),
						std::bind<void>(
							ref(*this),
							aout_stderr,
							"StdErr",
							_1,
							_2));
				}
			}
		}

		void operator ()(asio::streambuf &, error_code, size_t) {
		}

		void operator ()(std::list<stream_descriptor_pair>::iterator p, const char *msg, error_code ec, size_t) {
			auto &pipe = p->stream;
			auto &rbuf = p->buf;
			std::vector<char> data(BUFSIZ);
			{
				data.resize(asio::buffer_copy(asio::buffer(data), rbuf.data()));
				rbuf.consume(data.size());
				if (data.empty() || ec) {
					pipes.erase(p);
					check_finish();
					return;
				}
			}
			const auto str = ([&]() -> string {
				const auto line = encode_qp(data);
				std::stringstream ss;
				ss << msg << ' ' << line.length() << ':' << line << '\n';
				return ss.str();
			})();
			std::cout << str << std::flush;
			aio.post([this, str] {
				error_code ec;
				asio::write(sock, asio::buffer(str), ec);
			});
			asio::async_read_until(pipe, rbuf, end_read_condition(), std::bind<void>(ref(*this), p, msg, _1, _2));
		}

		compiler_bridge(tcp::acceptor &acc): aio(), sock(aio)
		{
			acc.accept(sock);
			do {
				try {
					workdir = opendir(mkdtemp("wandboxXXXXXX"));
				} catch (std::system_error &e) {
					if (e.code().value() != ENOTDIR) throw;
				}
			} while (!workdir && errno == ENOTDIR);
			cc_pid = 0;
			prog_pid = 0;
		}
	private:
		asio::io_service aio;
		tcp::socket sock;
		shared_ptr<DIR> workdir;
		list<stream_descriptor_pair> pipes;
		std::map<std::string, std::string> received;
		int cc_pid, prog_pid;
	};

	struct listener {
		template <typename ...Args>
		void operator ()(Args &&...args) {
			asio::io_service aio;
			tcp::endpoint ep(std::forward<Args>(args)...);
			tcp::acceptor acc(aio, ep);
			while (1) {
				shared_ptr<compiler_bridge> pcb(make_shared<compiler_bridge>(ref(acc)));
				std::thread([pcb] { (*pcb)(); }).detach();
			}
		}
		listener() {
			try {
				mkdir(config.jail.basedir, 0700);
			} catch (std::system_error &e) {
				if (e.code().value() != EEXIST) throw;
			}
			basedir = opendir(config.jail.basedir);
			if (::fchdir(::dirfd(basedir.get())) < 0) throw_system_error(errno);
		}
	private:
		shared_ptr<DIR> basedir;
	};
}
int main(int argc, char **argv) {
	using namespace wandbox;

	{
		namespace po = boost::program_options;

		{
			string config_file_raw;
			po::options_description opt("options");
			opt.add_options()
				("help,h", "show this help")
				("config,c", po::value<string>(&config_file_raw)->default_value(string(DATADIR) + "/config"), "specify config file")
			;
			
			po::variables_map vm;
			po::store(po::parse_command_line(argc, argv, opt), vm);
			po::notify(vm);

			if (vm.count("help")) {
				std::cout << opt << std::endl;
				return 0;
			}

			config_file = realpath(config_file_raw);
			if (config_file.empty()) {
				std::cerr << "config file '" << config_file_raw << "'not found" << std::endl;
				return 1;
			}
		}
		std::ifstream f(config_file);
		config = load_config(f);
	} {
		ptracer = realpath(dirname(realpath("/proc/self/exe")) + "/" + config.jail.exe);
	}
	listener s;
	s(boost::asio::ip::tcp::v4(), config.network.listen_port);
}
