#define BOOST_ERROR_CODE_HEADER_ONLY

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
			qi::parse(ite, end, qi::as_string[qi::repeat(len)[qi::char_]][phx::ref(d.second) = qi::_1] >> qi::eoi);
	}

//	inline string encode_qp(std::ostream &r) {
//		r << karma::format(karma::repeat(1, 76)[karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
//	}

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

	inline void push_back_strdup_all(vector<char *> &) { }
	template <typename A, typename ...Args>
	void push_back_strdup_all(vector<char *> &argv, A &&a, Args &&...args) {
		argv.push_back(strdup(a));
		push_back_strdup_all(argv, std::forward<Args>(args)...);
	}
	template <typename ...Args>
	child_process piped_spawn(int mode, const char *cmdname, Args &&...args) {
		vector<char *> argv;
		push_back_strdup_all(argv, args...);
		argv.push_back(0);
		if (mode == _P_OVERLAY) {
			::execv(cmdname, argv.data());
			exit(-1);
		}
		int pipe_stdin[2], pipe_stdout[2], pipe_stderr[2];
		pipe(pipe_stdin);
		pipe(pipe_stdout);
		pipe(pipe_stderr);
		const pid_t pid = ::fork();
		if (pid == 0) {
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
			::execv(cmdname, argv.data());
			abort();
		} else if (pid > 0) {
			close(pipe_stdin[0]);
			close(pipe_stdout[1]);
			close(pipe_stderr[1]);
			
			child_process child { pid, pipe_stdin[1], pipe_stdout[0], pipe_stderr[0] };
			std::for_each(argv.begin(), argv.end(), ::free);
			int st;
			if (mode ==  _P_WAIT) {
				::waitpid(pid, &st, 0);
				child.pid = st;
				return child;
			}
			return child;
		} 
		std::for_each(argv.begin(), argv.end(), ::free);
		return child_process { -1, -1, -1, -1 };
	}
}

namespace wandbox {

	struct thread_compare {
		bool operator ()(const std::thread &x, const std::thread &y) const {
			return x.get_id() < y.get_id();
		}
	};

	struct compiler_bridge {
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
			std::map<std::string, std::string> received;

			while (true) {
				asio::read_until(sock, rbuf, '\n');
				std::pair<string, string> d;
				std::istream rd(&rbuf);
				string line;
				if (getline(rd, line) && parse_line(line, d)) {
					std::cout << "command: " << d.first << " : " << d.second << std::endl;
					if (d.first == "Control" && d.second == "run") break;
					received[d.first] += decode_qp(d.second);
				}
			}

			const string srcname = "prog.cpp";
			shared_ptr<FILE> fp(::fopen(srcname.c_str(), "w"), [srcname](FILE *fp) {
				::unlink(srcname.c_str());
			});
			auto &s = received["Source"];
			::fwrite(s.data(), 1, s.length(), fp.get());
			::fclose(fp.get());
			std::cout << "Source <<EOF\n" << s << "\nEOF" << std::endl;

			const child_process c = piped_spawn(_P_NOWAIT, "/usr/bin/gcc", "/usr/bin/gcc", "-v", srcname.c_str());
			std::cout << "gcc : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;

			asio::async_read_until(sock, rbuf, '\n', std::bind(ref(*this), ref(rbuf), _1, _2));

			pipes.emplace_front(ref(aio), c.fd_stdout);
			auto cc_stdout = pipes.begin();
			asio::async_read_until(cc_stdout->stream, cc_stdout->buf, '\n',
								   std::bind(ref(*this),
											 cc_stdout,
											 "CompilerMessageS",
											 _1,
											 _2));

			pipes.emplace_front(ref(aio), c.fd_stderr);
			auto cc_stderr = pipes.begin();
			asio::async_read_until(cc_stderr->stream, cc_stderr->buf, '\n',
								   std::bind(ref(*this),
											 cc_stderr,
											 "CompilerMessageE",
											 _1,
											 _2));

			sig.async_wait(std::bind(ref(*this), c.pid, _1, _2));

			aio.run();
		}

		void operator ()(asio::streambuf &, error_code, size_t) {
		}

		void operator ()(int pid, error_code, int) {
			::waitpid(pid, 0, WNOHANG);
			const string progname = "./a.out";
			child_process c = piped_spawn(_P_NOWAIT, progname.c_str(), progname.c_str());
			std::cout << "a.out : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;

			pipes.emplace_front(ref(aio), c.fd_stdout);
			auto aout_stdout = pipes.begin();
			asio::async_read_until(aout_stdout->stream, aout_stdout->buf, '\n',
								   std::bind(ref(*this),
											 aout_stdout,
											 "StdOut",
											 _1,
											 _2));

			pipes.emplace_front(ref(aio), c.fd_stderr);
			auto aout_stderr = pipes.begin();
			asio::async_read_until(aout_stderr->stream, aout_stderr->buf, '\n',
								   std::bind(ref(*this),
											 aout_stderr,
											 "StdErr",
											 _1,
											 _2));

			pid = c.pid;
			sig.async_wait([pid, this](error_code, int) {
				::waitpid(pid, 0, WNOHANG);
				aio.post([this] {
					error_code ec;
					const string str = "Control 6:Finish\n";
					asio::write(sock, asio::buffer(str), ec);
					sock.close();
					aio.stop();
					std::cout << "closed" << std::endl;
				});
			});
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
			asio::async_read_until(pipe, rbuf, '\n', std::bind(ref(*this), p, msg, _1, _2));
		}

		compiler_bridge(tcp::endpoint ep)
			 : sock(aio), sig(aio, SIGCHLD)
		{
			do {
				char workdirnamebase[] = "wandboxXXXXXX";
				workdirname = ::mkdtemp(workdirnamebase);
				workdir.reset(::opendir(workdirnamebase), ::closedir);
			} while (!workdir && errno == ENOTDIR);
			if (!workdir) throw std::runtime_error("cannot open temporary directory");
			::fchdir(::dirfd(workdir.get()));
			tcp::acceptor acc(aio, ep);
			acc.accept(sock);
		}
	private:
		asio::io_service aio;
		tcp::socket sock;
		asio::signal_set sig;
		shared_ptr<DIR> workdir;
		list<stream_descriptor_pair> pipes;
		string workdirname;
	};

	struct listener {
		template <typename ...Args>
		void operator ()(Args &&...args) {
			while (1) {
				compiler_bridge cb({std::forward<Args>(args)...});
				cb();
			}
		}
		listener()
			 : basedir((::mkdir("/tmp/wandbox", 0700), ::opendir("/tmp/wandbox")), &::closedir)
		{
			if (!basedir) throw std::runtime_error("cannot open working dir");
			::fchdir(::dirfd(basedir.get()));
		}
	private:
		shared_ptr<DIR> basedir;
	};
}
int main(int, char **) {
	using namespace wandbox;
	listener s;
	s(boost::asio::ip::tcp::v4(), listen_port);
}
