#define BOOST_ERROR_CODE_HEADER_ONLY

#include "common.hpp"
#include <map>
#include <string>
#include <vector>
#include <array>
#include <thread>
#include <mutex>
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
	using std::string;
	using std::vector;
	using std::array;
	using std::size_t;
	using std::move;
	using boost::system::error_code;

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

	template <typename Range, typename std::enable_if<std::is_convertible<typename boost::range_category<Range>::type, std::bidirectional_iterator_tag>::value>::type *& = enabler>
	string decode_qp(const Range &r) {
		auto ite = r.begin();
		const auto end = r.end();
		string ret;
		qi::parse(ite, end, *((qi::char_ - '=') | (qi::lit("=\n")) | (qi::lit('=') > qi::uint_parser<char, 16, 2, 2>())), ret);
		return ret;
	}
	template <typename Range>
//	auto encode_qp(const Range &r) -> typename std::enable_if<std::is_convertible<typename std::iterator_traits<decltype(boost::begin(r))>::iterator_category, std::bidirectional_iterator_tag>::value, string>::type {
	string encode_qp(const Range &r) {
		if (r.begin() == r.end()) return {};
		string ret;
		karma::generate(back_inserter(ret), karma::repeat(1, 76)[karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
		return ret;
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

	template <typename Fn>
	intptr_t _spawnvX(Fn execer, int mode, const char *cmdname, const char *const *argv_) {
		vector<char *> argv;
		for (const char *const *p = argv_; *p; ++p) {
			argv.push_back(::strdup(*p));
		}
		if (mode == _P_OVERLAY) {
			execer(cmdname, argv.data());
			exit(-1);
		}
		const pid_t pid = ::fork();
		if (pid == 0) {
			execer(cmdname, argv.data());
			exit(-1);
		} else if (pid > 0) {
			std::for_each(argv.begin(), argv.end(), ::free);
			int st;
			if (mode ==  _P_WAIT) {
				::waitpid(pid, &st, 0);
				return st;
			}
			return pid;
		} 
		std::for_each(argv.begin(), argv.end(), ::free);
		return -1;
	}
	template <typename Fn>
	intptr_t _spawnlX2(Fn execer, int mode, const char *cmdname, vector<const char *> &&v) {
		v.push_back(nullptr);
		return _spawnvX(execer, mode, cmdname, v.data());
	}
	template <typename Fn, typename ...Args>
	intptr_t _spawnlX2(Fn execer, int mode, const char *cmdname, vector<const char *> &&v, const char *arg0, Args &&...args) {
		v.push_back(arg0);
		return _spawnlX2(execer, mode, cmdname, move(v), std::forward<Args>(args)...);
	}
	template <typename Fn, typename ...Args>
	intptr_t _spawnlX(Fn execer, int mode, const char *cmdname,  Args &&...args) {
		return _spawnlX2(execer, mode, cmdname, {}, std::forward<Args>(args)...);
	}
	intptr_t _spawnv(int mode, const char *cmdname, const char *const *argv) {
		return _spawnvX(&::execv, mode, cmdname, argv);
	}
//	intptr_t _spawnve(int mode, const char *cmdname, const char *const *argv) {
//		return _spawnvX(&::execve, mode, cmdname, argv);
//	}
	intptr_t _spawnvp(int mode, const char *cmdname, const char *const *argv) {
		return _spawnvX(&::execvp, mode, cmdname, argv);
	}
//	intptr_t _spawnvpe(int mode, const char *cmdname, const char *const *argv) {
//		return _spawnvX(&::execvpe, mode, cmdname, argv);
//	}
	template <typename ...Args>
	intptr_t _spawnl(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execv, mode, cmdname, std::forward<Args>(args)...);
	}
//	template <typename ...Args>
//	intptr_t _spawnle(int mode, const char *cmdname, Args &&...args) {
//		return _spawnlX(&::execve, mode, cmdname, std::forward<Args>(args)...);
//	}
	template <typename ...Args>
	intptr_t _spawnlp(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execvp, mode, cmdname, std::forward<Args>(args)...);
	}
//	template <typename ...Args>
//	intptr_t _spawnlpe(int mode, const char *cmdname, Args &&...args) {
//		return _spawnlX(&::execvpe, mode, cmdname, std::forward<Args>(args)...);
//	}

	struct pipe_t {
		explicit pipe_t(int flags = 0) {
			if (pipe2(pipefd, flags) == -1) throw std::runtime_error("pipe creation failed");
		}
		~pipe_t() {
			close(pipefd[0]);
			close(pipefd[1]);
		}
		int rfd() const { return pipefd[0]; }
		int wfd() const { return pipefd[1]; }
		pipe_t(const pipe_t &) = delete;
		pipe_t &operator =(const pipe_t &) = delete;
		int pipefd[2];
	};

	inline void set_cloexec(int fd) {
		fcntl(fd, F_SETFD, (long)FD_CLOEXEC);
	}
	inline int dup_cloexec(int fd) {
		return fcntl(fd, F_DUPFD_CLOEXEC, (long)0);
	}

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
/*
		pipe_t child_stdin(O_CLOEXEC), child_stdout(O_CLOEXEC), child_stderr(O_CLOEXEC);
		int parent_stdin = dup_cloexec(0);
		int parent_stdout = dup_cloexec(1);
		int parent_stderr = dup_cloexec(2);
		print(std::cout, "spawn : ", args...) << std::endl;
		dup2(child_stdin.rfd(), 0);
		dup2(child_stdout.wfd(), 1);
		dup2(child_stderr.wfd(), 2);
		child_process child;
		child.pid = _spawnlX(execv, mode, std::forward<Args>(args)...);
		dup2(parent_stdin, 0);
		dup2(parent_stdout, 1);
		dup2(parent_stderr, 2);
		close(parent_stdin);
		close(parent_stdout);
		close(parent_stderr);
		child.fd_stdin = dup_cloexec(child_stdin.wfd());
		child.fd_stdout = dup_cloexec(child_stdout.rfd());
		child.fd_stderr = dup_cloexec(child_stderr.rfd());
		return child;
	*/
	}
}

#include "yield.hpp"

namespace wandbox {

	struct compiler_bridge: coroutine {
		template <typename Fn>
		void operator ()(const std::string &msg, std::shared_ptr<asio::posix::stream_descriptor> pipe, Fn fn) try {
			while (1) {
				error_code ec;
				asio::streambuf rbuf, wbuf;
				const auto len = asio::read_until(*pipe, rbuf, '\n', ec);
				std::cout << "pipe : " << msg << " : " << len << ", " << ec.message() << std::endl;
				rbuf.commit(len);
				std::istream rd(&rbuf);
				string line;
				getline(rd, line);
				if (rd) line += ('\n');
				const string so = encode_qp(line);
				std::cout << "'" << so << "'" << std::endl;
				if (!so.empty()) {
					std::ostream wr(&wbuf);
					wr << msg << " " << so.length() << " : " << so << '\n';
					std::mutex mut;
					std::condition_variable cond;
					std::unique_lock<std::mutex> lk(mut);
					aio->post([&, this] {
						error_code ec;
						asio::write(*sock, wbuf, ec);
						cond.notify_one();
					});
					cond.wait(lk);
				}
				if (ec) break;
			}
			error_code ec;
			pipe->close(ec);
			if (fn) fn(std::move(*this));
		} catch (...) {
			std::cout << "catch..." << std::endl;
		}
		void operator ()(error_code ec = error_code(), std::size_t length = 0) try {
			reenter (this) {
				std::cout << "listen" << std::endl;
				fork compiler_bridge(*this)();

				if (is_parent()) {
					aio->run();
					return;
				}

				do {
					sock = std::make_shared<tcp::socket>(*aio);
					yield acc->async_accept(*sock, move(*this));
					fork compiler_bridge(*this)();
				} while (is_parent()) ;
				std::cout << "accepted" << std::endl;

				while (true) {
					yield asio::async_read_until(*sock, bufs->sock.read, '\n', move(*this));
					std::pair<string, string> d;
					bufs->sock.read.commit(length);
					std::istream rd(&bufs->sock.read);
					string line;
					if (getline(rd, line) && parse_line(line, d)) {
						std::cout << "command: " << d.first << " : " << d.second << std::endl;
						if (d.first == "Control" && d.second == "run") break;
						received[d.first] += decode_qp(d.second);
					}
				}

				do {
					char tmpdirnamebase[] = "/tmp/wandbox/wandboxXXXXXX";
					tmpdirname = ::mkdtemp(tmpdirnamebase);
					tmpdir.reset(::opendir(tmpdirnamebase), ::closedir);
				} while (!tmpdir && errno == ENOTDIR);
				if (!tmpdir) throw std::runtime_error("cannot open temporary directory");
				::fchdir(::dirfd(tmpdir.get()));

				std::cout << "set tmpdir to " << tmpdirname << std::endl;

				{
					const auto srcname = tmpdirname + "/prog.cpp";
					{
						shared_ptr<FILE> fp(::fopen(srcname.c_str(), "w"), [srcname](FILE *fp) {
							//						::unlink(srcname.c_str());
							::fclose(fp);
						});
						auto &s = received["Source"];
						::fwrite(s.data(), 1, s.length(), fp.get());
						::fflush(fp.get());
						std::cout << "Source <<EOF\n" << s << "\nEOF" << std::endl;
					}

					const child_process c = piped_spawn(_P_NOWAIT, "/usr/bin/gcc", "/usr/bin/gcc", "-v", srcname.c_str());
					std::cout << "gcc : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
					child_pid = c.pid;

					std::thread thr_stdout(
						*this,
						"CompilerMessage",
						std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdout),
						[](compiler_bridge &&self) {
							::sleep(1);
							error_code ec;
							self.sock->cancel(ec);
						});
					thr_stdout.detach();
					std::thread thr_stderr(
						*this,
						"CompilerMessage",
						std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stderr),
						[](ignore_param){
							::sleep(1);
						});
					thr_stderr.detach();
				}

				while (1) {
					yield asio::async_read_until(*sock, bufs->sock.read, '\n', move(*this));
					if (ec == asio::error::operation_aborted) break;
					std::pair<string, string> d;
					bufs->sock.read.commit(length);
					std::istream rd(&bufs->sock.read);
					string line;
					if (getline(rd, line) && parse_line(line, d)) {
					}
				}
				if (child_pid > 0) {
					//::kill(child_pid, SIGTERM);
					//::kill(child_pid, SIGKILL);
					{
						::waitpid(child_pid, 0, 0);
					}
				}

				sleep(5);

				{
					const auto progname = tmpdirname + "/a.out";
					const child_process c = piped_spawn(_P_NOWAIT, progname.c_str(), progname.c_str());
					child_pid = c.pid;
					std::cout << "a.out : { " << c.pid << ", " << c.fd_stdin << ", " << c.fd_stdout << ", " << c.fd_stderr << " }" << std::endl;
					//pipe = std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdin);
					std::thread thr_stdout(
						*this,
						"StdOut",
						std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdout),
						[](compiler_bridge &&self) {
							::sleep(1);
							error_code ec;
							self.sock->cancel(ec);
						});
					thr_stdout.detach();
					std::thread thr_stderr(
						*this,
						"StdErr",
						std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stderr),
						[](ignore_param) {
							::sleep(1);
						});
					thr_stderr.detach();
				}

				while (1) {
					yield asio::async_read_until(*sock, bufs->sock.read, '\n', move(*this));
					if (ec == asio::error::operation_aborted) break;
					std::pair<string, string> d;
					bufs->sock.read.commit(length);
					std::istream rd(&bufs->sock.read);
					string line;
					if (getline(rd, line) && parse_line(line, d)) {
					}
				}
				if (child_pid > 0) {
					//::kill(child_pid, SIGTERM);
					//::kill(child_pid, SIGKILL);
					{
						::waitpid(child_pid, 0, 0);
					}
				}

				std::cout << "closed" << std::endl;
			}
		} catch (...) {
			std::cout << "catch..." << std::endl;
			error_code ec;
			sock->close(ec);
		}
		struct bufs_t {
			struct {
				asio::streambuf read;
				asio::streambuf write;
			} sock, pipe;
		};
		compiler_bridge(short port)
			 : aio(std::make_shared<asio::io_service>()),
			   acc(std::make_shared<tcp::acceptor>(*aio, tcp::endpoint(tcp::v4(), port))),
			   bufs(std::make_shared<bufs_t>())
		{
			::mkdir("/tmp/wandbox", 0700);
			basedir.reset(::opendir("/tmp/wandbox"), &::closedir);
			if (!basedir) throw std::runtime_error("cannot open working dir");
		}
	private:
		static shared_ptr<vector<asio::posix::stream_descriptor>> make_null_fds() {
			return std::make_shared<vector<asio::posix::stream_descriptor>>();
		}

		shared_ptr<asio::io_service> aio;
		shared_ptr<tcp::acceptor> acc;
		shared_ptr<asio::posix::stream_descriptor> pipe;
		shared_ptr<tcp::socket> sock;
		shared_ptr<DIR> basedir;
		shared_ptr<DIR> tmpdir;
		string tmpdirname;
		shared_ptr<bufs_t> bufs;
		int child_pid;
		std::map<std::string, std::string> received;
	};
}
int main(int, char **) {
	using namespace wandbox;
	asio::io_service aio;

	compiler_bridge s(listen_port);
	s();
}
