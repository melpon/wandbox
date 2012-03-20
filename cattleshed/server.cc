#include "common.hpp"
#include "yield.hpp"
#include <map>
#include <string>
#include <vector>
#include <array>
#include <boost/range.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/istream_range.hpp>
#include <boost/fusion/include/std_pair.hpp>

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

	template <typename R, typename EndFinder>
	inline R make_partial_range(R r, const EndFinder &fn) { return R(boost::begin(r), fn(r)); }



	template <typename Range>
	size_t parse_line(const Range &r, std::pair<string, string> &d) {
		auto ite = boost::begin(r);
		const auto end = boost::end(r);
		int len;
		if (qi::parse(ite, end, qi::as_string[+qi::alpha][phx::ref(d.first) = qi::_1] >> *qi::space >> qi::int_[phx::ref(len) = qi::_1] >> ':') &&
			qi::parse(ite, end, qi::as_string[qi::repeat(len)[qi::char_]][phx::ref(d.second) = qi::_1] >> '\n')) return std::distance(boost::begin(r), ite);
		return 0;
	}

	template <typename Range>
	string decode_qp(const Range &r) {
		auto ite = r.begin();
		const auto end = r.end();
		string ret;
		qi::parse(ite, end, *((qi::char_ - '=') | (qi::lit("=\n")) | (qi::lit('=') > qi::uint_parser<char, 16, 2, 2>())), ret);
		return ret;
	}
	template <typename Range>
	string encode_qp(const Range &r) {
		if (r.begin() == r.end()) return {};
		string ret;
		karma::generate(back_inserter(ret), karma::repeat(1, 76)[karma::print | ('=' << karma::upper[karma::right_align(2, '0')[karma::uint_generator<unsigned char, 16>()]])] % "=\n", r);
		return ret;
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

	template <typename Fn>
	intptr_t _spawnvX(Fn execer, int mode, const char *cmdname, const char *const argv) {
		if (mode == _P_OVERLAY) execer(cmdname, argv);
		const pid_t pid = fork();
		if (pid == 0) {
			execer(cmdname, argv);
		} else if (pid > 0) {
			int st;
			if (mode ==  _P_WAIT) {
				wait(pid, &st, 0);
				return st;
			}
			return pid;
		} else {
			return -1;
		}
	}
	template <typename Fn>
	intptr_t _spawnlX2(Fn execer, int mode, const char *cmdname, vector<const char *> &&v) {
		v.push_back(nullptr);
		return _spawnvX(execer, mode, cmdname, v.data());
	}
	template <typename Fn, typename ...Args>
	intptr_t _spawnlX2(Fn execer, int mode, const char *cmdname, vector<const char *> &&v, const char *arg0, coArgs &&...args) {
		v.push_back(arg0);
		return _spawnlX2(execer, mode, cmdname, move(v), std::forward<Args>(args)...);
	}
	template <typename Fn, typename ...Args>
	intptr_t _spawnlX(Fn execer, int mode, const char *cmdname,  Args &&...args) {
		return _spawnlX2(execer, mode, cmdname, {}, std::forward<Args>(args)...);
	}
	intptr_t _spawnv(int mode, const char *cmdname, const char *const argv) {
		return _spawnvX(&::execv, mode, cmdname, argv);
	}
	intptr_t _spawnve(int mode, const char *cmdname, const char *const argv) {
		return _spawnvX(&::execve, mode, cmdname, argv);
	}
	intptr_t _spawnvp(int mode, const char *cmdname, const char *const argv) {
		return _spawnvX(&::execvp, mode, cmdname, argv);
	}
	intptr_t _spawnvpe(int mode, const char *cmdname, const char *const argv) {
		return _spawnvX(&::execvpe, mode, cmdname, argv);
	}
	template <typename ...Args>
	intptr_t _spawnl(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execv, mode, cmdname, std::forward<Args>(args)...);
	}
	template <typename ...Args>
	intptr_t _spawnle(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execve, mode, cmdname, std::forward<Args>(args)...);
	}
	template <typename ...Args>
	intptr_t _spawnlp(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execvp, mode, cmdname, std::forward<Args>(args)...);
	}
	template <typename ...Args>
	intptr_t _spawnlpe(int mode, const char *cmdname, Args &&...args) {
		return _spawnlX(&::execvpe, mode, cmdname, std::forward<Args>(args)...);
	}

	struct pipe_t {
		pipe_t(int flags = 0) {
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
		fcntl(fd, F_SETFD, FD_CLOEXEC);
	}
	inline int dup_cloexec(int fd) {
		return fcntl(fd, F_DUPFD_CLOEXEC, 0);
	}

	struct child_process {
		intptr_t pid;
		int fd_stdin;
		int fd_stdout;
		int fd_stderr;
	};
	struct unique_fd {
		unique_fd(int fd): fd(fd) { }
		~uniquef_fd() { close(); }
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
	child_process piped_spawn() {
		pipe_t child_stdin{O_CLOEXEC}, child_stdout{O_CLOEXEC}, child_stderr{O_CLOEXEC};
		int parent_stdin = dup_cloexec(0);
		int parent_stdout = dup_cloexec(1);
		int parent_stderr = dup_cloexec(2);
		dup2(child_stdin.rfd(), 0);
		dup2(child_stdout.wfd(), 1);
		dup2(child_stderr.wfd(), 2);
		child_process child;
		child.pid = spawn();
		dup2(parent_stdin, 0);
		dup2(parent_stdout, 1);
		dup2(parent_stderr, 2);
		child.fd_stdin = dup_cloexec(child_stdin.wfd());
		child.fd_stdout = dup_cloexec(child_stdout.rfd());
		child.fd_stderr = dup_cloexec(child_stderr.rfd());
		return child;
	}

	struct compiler_bridge: coroutine {
		void start() {
			std::cout << "accept" << std::endl;
			(*this)(shared_from_this());
		}
		void operator ()(error_code ec = error_code(), std::size_t length = 0) {
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
					if (!ec) fork compiler_bridge(*this)();
				} while (is_parent()) ;

				while (true) {
					yield asio::async_read_until(sock, bufs.sock.read, '\n', move(*this));
					std::pair<string, string> d;
					bufs.sock.read.commit(l);
					std::istream rd(&bufs.sock.read);
					if (size_t l = parse_line(brange::istream_range<char>(rd))) {
						if (d.first == "Control" && d.second == "run") break;
						received[d.first] += decode_qp(d.second);
					}
				}

				do {
					char tmpdirnamebase[] = "/tmp/wandbox/wandboxXXXXXX";
					::tmpdirname = mkdtemp(tmpdirnamebase);
					tmpdir.reset(::opendir(tmpdirnamebase));
				} while (!tmpdir && errno == ENOTDIR);
				if (!tmpdir) throw std::runtime_error("cannot open temporary directory");

				for (stage = 0; stage < 2; ++stage) {
					{
						const child_process c = ([](const vector<string> &args) -> child_process {
							vector<const char *> a(args.size() + 1);
							std::transform(args.begin(), args.end(), a.begin(), [](const string &s) { return s.c_str(); });
							return piped_spawn(_P_NOWAIT, args.top().c_str(), a.data());
						})(stage == 0 ? ([this]() -> vector<string> {
							unique_ptr<FILE, void (FILE *)> fp(::fopen((tmpdir + "/prog.cpp").c_str(), "w"), [](FILE *fp) {
								::unlink(::fileno(fp));
								::fclose(fp);
							});

							auto &s = received["Source"];
							::fwrite(s.begin(), 1, s.length(), fd.get());
							::fflush(fd.get());

							return {"/usr/bin/gcc", tmpdir + "/prog.cpp"};
						})() : vector<string>{{tmpdir + "/prog.exe"}});

						child_pid = c.pid;
						fds.emplace_back(std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdin));
						fds.emplace_back(std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stdout));
						fds.emplace_back(std::make_shared<asio::posix::stream_descriptor>(*aio, c.fd_stderr));
					}

					fork compiler_bridge(*this)();
					if (is_child()) {
						child_pid = 0;
						fds[1] = fds[2];
					}
					fds.pop_back();

					while (1) {
						yield asio::async_read_until(*fds[1], bufs.pipe.read, '\n', move(*this));
						if (ec) {
							if (child_pid == 0) return;
							break;
						} else {
							bufs.pipe.read.commit(length);
							std::istream rd(&bufs.pipe.read);
							const string so = encode_qp(rd);
							std::ostream wr(&bufs.sock.write);
							wr << (stage == 0 ? "CompilerMessage " : (child_pid == 0 ? "StdErr " : "StdOut ")) << so.length() << " : " << so;
						}
						yield asio::async_write(*sock, bufs.sock.write, move(*this));
						bufs.sock.write.consume(length);
					}
					fds.clear();
					wait(child_pid);
				}

				print_map(received);
				{
					std::ostream wr(&write_sb);
					const string so = encode_qp(received["Source"]);
					wr << "StdOut " << so.length() << ':' << so << '\n';
				}
				yield asio::async_write(sock, write_sb, std::bind(std::ref(*this), shared_from_this(), _1, _2));
				write_sb.consume(length);
				std::cout << "closed" << std::endl;
			}
		}
		compiler_bridge(short port)
			 : aio(std::make_shared<asio::io_service>()),
			   acc(aio, tcp::endpoint(tcp::v4(), port))
		{
			::mkdir("/tmp/wandbox", 0700);
			basedir.reset(::opendir("/tmp/wandbox"));
			if (!basedir) throw std::runtime_error("cannot open working dir");
		}
	private:
		static shared_ptr<vector<asio::posix::stream_descriptor>> make_null_fds() {
			return std::make_shared<vector<asio::posix::stream_descriptor>>();
		}

		shared_ptr<asio::io_service> aio;
		shared_ptr<tcp::acceptor> acc;
		vector<shared_ptr<asio::posix::stream_descriptor>> fds;
		shared_ptr<tcp::socket> sock;
		shared_ptr<DIR> basedir;
		shared_ptr<DIR> tmpdir;
		string tmpdirname;
		struct {
			struct {
				asio::streambuf read;
				asio::streambuf write;
			} sock, pipe;
		} bufs;
		int child_pid;
		std::map<std::string, std::string> received;
	};
}
int main(int argc, char **) {
	using namespace wandbox;
	asio::io_service aio;

	server s(aio, listen_port);
	aio.run();
}
