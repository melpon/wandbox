//#define BOOST_ERROR_CODE_HEADER_ONLY

#include "common.hpp"
#include <map>
#include <string>
#include <vector>
#include <array>
#include <thread>
#include <mutex>
#include <sstream>
#include <set>
#include <functional>
#include <list>
#include <condition_variable>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/istream_range.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/algorithm/string/split.hpp>

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <dirent.h>
#include <sys/wait.h>

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


	extern void *enabler;

	template <typename Range, typename std::enable_if<std::is_convertible<typename boost::range_category<Range>::type, std::bidirectional_iterator_tag>::value>::type *& = enabler>
	bool parse_line(const Range &r, std::pair<string, string> &d) {
		auto ite = boost::begin(r);
		const auto end = boost::end(r);
		int len;
		return qi::parse(ite, end, qi::as_string[+qi::alpha][phx::ref(d.first) = qi::_1] >> *qi::space >> qi::int_[phx::ref(len) = qi::_1] >> ':') &&
			qi::parse(ite, end, qi::as_string[qi::repeat(len)[qi::char_]][phx::ref(d.second) = qi::_1] >> qi::eol >> qi::eoi);
	}

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
		if (mode == _P_OVERLAY) {
			::fchdir(::dirfd(workdir));
			std::cout << "exec error : " << ::execv(x.front().data(), a.data()) << ',';
			std::cout << strerror(errno) << std::endl;
			exit(-1);
		}
		int pipe_stdin[2], pipe_stdout[2], pipe_stderr[2];
		pipe(pipe_stdin);
		pipe(pipe_stdout);
		pipe(pipe_stderr);
		const pid_t pid = ::fork();
		if (pid == 0) {
			::fchdir(::dirfd(workdir));
			dup2(pipe_stdin[0], 0);
			dup2(pipe_stdout[1], 1);
			dup2(pipe_stderr[1], 2);
			close(pipe_stdin[0]);
			close(pipe_stdin[1]);
			close(pipe_stdout[0]);
			close(pipe_stdout[1]);
			close(pipe_stderr[0]);
			close(pipe_stderr[1]);
			fcntl(0, F_SETFD, (long)0);
			fcntl(1, F_SETFD, (long)0);
			fcntl(2, F_SETFD, (long)0);
			std::cout << "exec error : " << ::execv(x.front().data(), a.data()) << ',';
			std::cout << strerror(errno) << std::endl;
			exit(-1);
		} else if (pid > 0) {
			close(pipe_stdin[0]);
			close(pipe_stdout[1]);
			close(pipe_stderr[1]);

			child_process child { pid, pipe_stdin[1], pipe_stdout[0], pipe_stderr[0] };
			if (mode == _P_WAIT) {
				int st;
				::waitpid(pid, &st, 0);
				child.pid = st;
				return child;
			}
			return child;
		} 
		return child_process { -1, -1, -1, -1 };
	}
}

namespace wandbox {

	string ptracer;

	struct thread_compare {
		bool operator ()(const std::thread &x, const std::thread &y) const {
			return x.get_id() < y.get_id();
		}
	};

	struct compiler_bridge {
		string get_srcname() const {
			return "prog.cpp";
		}
		string get_progname() const {
			return "prog.exe";
		}
		vector<string> get_runargs() const {
			return { "./" + get_progname() };
		}
		bool is_any_of(const string& str, const string& value) const {
			vector<string> result;
			boost::algorithm::split(result, str, [](char c) { return c == ','; });
			return std::any_of(result.begin(), result.end(), [&](const string& v) { return v == value; });
		}
		bool has_optimization() const {
			auto it = received.find("CompilerOption");
			return it != received.end() && is_any_of(it->second, "optimize");
		}
		bool has_warning() const {
			auto it = received.find("CompilerOption");
			return it != received.end() && is_any_of(it->second, "warning");
		}
		vector<string> get_compiler_arg() const {
			vector<string> args;
			if (received.at("Control") == "compiler=gcc-head") {
				args = { "/usr/local/gcc-head/bin/g++", get_srcname(), "-std=c++11", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.8.1") {
				args = { "/usr/local/gcc-4.8.1/bin/g++", get_srcname(), "-std=c++11", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.7.3") {
				args = { "/usr/local/gcc-4.7.3/bin/g++", get_srcname(), "-std=c++11", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.6.4") {
				args = { "/usr/local/gcc-4.6.4/bin/g++", get_srcname(), "-std=c++0x", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.5.4") {
				args = { "/usr/local/gcc-4.5.4/bin/g++", get_srcname(), "-std=c++0x", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.4.7") {
				args = { "/usr/local/gcc-4.4.7/bin/g++", get_srcname(), "-std=c++0x", "-o", get_progname(), "-lpthread" };
			} else if (received.at("Control") == "compiler=gcc-4.3.6") {
				args = { "/usr/local/gcc-4.3.6/bin/g++", get_srcname(), "-std=c++0x", "-o", get_progname(), "-lpthread" };
			}
			if (has_optimization()) args.push_back("-O2");
			if (has_warning()) args.push_back("-Wall");

			//} else if (received.at("Control") == "compiler=clang-3.1") {
			//	args = { "/usr/local/llvm-3.1/bin/clang++", get_srcname(), "/usr/lib/libsupc++.a", "-stdlib=libc++", "-std=c++11", "-o", get_progname(), "-lpthread" };
			//	if (has_optimization()) args.push_back("-O2");
			//	if (has_warning()) args.push_back("-Wall");
			//} else if (received.at("Control") == "compiler=clang-3.2") {
			//	args = { "/usr/local/llvm-3.2/bin/clang++", get_srcname(), "/usr/lib/libsupc++.a", "-stdlib=libc++", "-std=c++11", "-o", get_progname(), "-lpthread" };
			//	if (has_optimization()) args.push_back("-O2");
			//	if (has_warning()) args.push_back("-Wall");
			//} else if (received.at("Control") == "compiler=ghc") {
			//	args = { "/usr/bin/ghc", get_srcname(), "-o", get_progname() };
			//	if (has_optimization()) args.push_back("-O2");
			//	if (has_warning()) args.push_back("-Wall");
			//} else if (received.at("Control") == "compiler=mcs") {
			//	args = { "/usr/bin/mcs", get_srcname(), "-out:" + get_progname() };
			//	if (has_optimization()) args.push_back("-optimize");
			//	//if (has_warning()) args.push_back("-warn:4"); // it is default.
			//}
			return args;
		}
		void send_version() {
			const auto proc = [](const vector<string>& args) -> std::string {
				shared_ptr<DIR> workdir(::opendir("/"), ::closedir);
				const child_process c = piped_spawn(_P_WAIT, workdir.get(), args);
				std::string out;
				char buf[1024];
				int r;
				while ((r = ::read(c.fd_stdout, buf, sizeof(buf))) > 0)
					out += std::string(buf, r);
				return out;
			};
			string line =
				"gcc-head,C++,gcc HEAD," + proc({ "/usr/local/gcc-head/bin/g++", "-dumpversion" }) +
				"gcc-4.8.1,C++,gcc," + proc({ "/usr/local/gcc-4.8.1/bin/g++", "-dumpversion" }) +
				"gcc-4.7.3,C++,gcc," + proc({ "/usr/local/gcc-4.7.3/bin/g++", "-dumpversion" }) +
				"gcc-4.6.4,C++,gcc," + proc({ "/usr/local/gcc-4.6.4/bin/g++", "-dumpversion" }) +
				"gcc-4.5.4,C++,gcc," + proc({ "/usr/local/gcc-4.5.4/bin/g++", "-dumpversion" }) +
				"gcc-4.4.7,C++,gcc," + proc({ "/usr/local/gcc-4.4.7/bin/g++", "-dumpversion" }) +
				"gcc-4.3.6,C++,gcc," + proc({ "/usr/local/gcc-4.3.6/bin/g++", "-dumpversion" }) +
				"";
				//"clang-3.1,C++,Clang,3.1\n" +
				//"clang-3.2,C++,Clang,3.2\n" +
				//"ghc,Haskell,ghc," + proc({ "/usr/bin/ghc", "--numeric-version" }) +
				//"mcs,C#,Mono,2.8\n";
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
		void operator ()() {
			// io thread
			asio::streambuf rbuf;

			{
				string buf;
				while (true) {
					asio::read_until(sock, rbuf, '\n');
					std::pair<string, string> d;
					{
						std::istream rd(&rbuf);
						string line;
						getline(rd, line);
						buf += line;
						buf += '\n';
					}
					if (parse_line(buf, d)) {
						std::cout << "command: " << d.first << " : " << d.second << std::endl;
						if (d.first == "Control" && d.second == "run") break;
						if (d.first == "Version") {
							send_version();
							return;
						}
						received[d.first] += decode_qp(d.second);
						buf.clear();
					}
				}
			}

			const string srcname = get_srcname();

			auto &s = received["Source"];
			{
				int fd = ::openat(::dirfd(workdir.get()), srcname.c_str(), O_WRONLY|O_CLOEXEC|O_CREAT|O_TRUNC, 0600);
				::write(fd, s.data(), s.length());
				::fsync(fd);
				::close(fd);
			}
			std::cout << "Source <<EOF\n" << s << "\nEOF" << std::endl;

			const child_process c = piped_spawn(_P_NOWAIT, workdir.get(), get_compiler_arg());
			std::cout << "gcc : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
			::close(c.fd_stdin);
			cc_pid = c.pid;

			asio::async_read_until(sock, rbuf, '\n', std::bind<void>(ref(*this), ref(rbuf), _1, _2));

			aio.post([this] {
				error_code ec;
				const string str = "Control 5:Start\n";
				asio::write(sock, asio::buffer(str), ec);
			});

			pipes.emplace_front(ref(aio), c.fd_stdout);
			auto cc_stdout = pipes.begin();
			asio::async_read_until(cc_stdout->stream, cc_stdout->buf, '\n',
								   std::bind<void>(ref(*this),
											 cc_stdout,
											 "CompilerMessageS",
											 _1,
											 _2));

			pipes.emplace_front(ref(aio), c.fd_stderr);
			auto cc_stderr = pipes.begin();
			asio::async_read_until(cc_stderr->stream, cc_stderr->buf, '\n',
								   std::bind<void>(ref(*this),
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
				if (code < -1) {
					const char *sig = ::strsignal(-code);
					if (not sig) sig = "";
					str += "Signal " + boost::lexical_cast<std::string>(::strlen(sig)) + ":" + sig + "\n";
				}
				const auto c = boost::lexical_cast<std::string>(code);
				str += "ExitCode " + boost::lexical_cast<std::string>(c.length()) + ":" + c + "\n";
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
					vector<string> runargs = get_runargs();
					runargs.insert(runargs.begin(), ptracer);
					child_process c = piped_spawn(_P_NOWAIT, workdir.get(), runargs);
					std::cout << "a.out : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
					prog_pid = c.pid;

					pipes.emplace_front(ref(aio), c.fd_stdout);
					auto aout_stdout = pipes.begin();
					asio::async_read_until(aout_stdout->stream, aout_stdout->buf, '\n',
										   std::bind<void>(ref(*this),
													 aout_stdout,
													 "StdOut",
													 _1,
													 _2));

					pipes.emplace_front(ref(aio), c.fd_stderr);
					auto aout_stderr = pipes.begin();
					asio::async_read_until(aout_stderr->stream, aout_stderr->buf, '\n',
										   std::bind<void>(ref(*this),
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
			std::istream is(&rbuf);
			string line;
			getline(is, line);
			if (is) line += '\n';
			if (!is && line.empty() && ec) {
				pipes.erase(p);
				check_finish();
				return;
			}
			line = encode_qp(line);
			const auto str = ([&]() -> string {
				std::stringstream ss;
				ss << msg << ' ' << line.length() << ':' << line << '\n';
				return ss.str();
			})();
			std::cout << str << std::flush;
			aio.post([this, str] {
				error_code ec;
				asio::write(sock, asio::buffer(str), ec);
			});
			asio::async_read_until(pipe, rbuf, '\n', std::bind<void>(ref(*this), p, msg, _1, _2));
		}

		compiler_bridge(asio::io_service &main_aio, tcp::acceptor &acc): main_aio(main_aio), aio(), sock(aio)
		{
			acc.accept(sock);
			do {
				char workdirnamebase[] = "wandboxXXXXXX";
				workdirname = ::mkdtemp(workdirnamebase);
				workdir.reset(::opendir(workdirnamebase), ::closedir);
			} while (!workdir && errno == ENOTDIR);
			if (!workdir) throw std::runtime_error("cannot open temporary directory");
			cc_pid = 0;
			prog_pid = 0;
		}
	private:
		asio::io_service &main_aio;
		asio::io_service aio;
		tcp::socket sock;
		shared_ptr<DIR> workdir;
		list<stream_descriptor_pair> pipes;
		string workdirname;
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
				shared_ptr<compiler_bridge> pcb(make_shared<compiler_bridge>(ref(aio), ref(acc)));
				std::thread([pcb] { (*pcb)(); }).detach();
			}
		}
		listener(): basedir((::mkdir("/tmp/wandbox", 0700), ::opendir("/tmp/wandbox")), &::closedir) {
			if (!basedir) throw std::runtime_error("cannot open working dir");
			::fchdir(::dirfd(basedir.get()));
		}
	private:
		shared_ptr<DIR> basedir;
	};
}
int main(int, char **) {
	using namespace wandbox;
	{
		unique_ptr<char, void(*)(void *)> cwd(::getcwd(nullptr, 0), &::free);
		ptracer = string(cwd.get()) + "/ptracer.exe";
	}
	listener s;
	s(boost::asio::ip::tcp::v4(), listen_port);
}
