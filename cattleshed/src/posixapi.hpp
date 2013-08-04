#include <memory>
#include <system_error>
#include <vector>

#include <dirent.h>
#include <fcntl.h>
#include <libgen.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>


namespace wandbox {
	struct unique_fd {
		explicit unique_fd(int fd): fd(fd) { }
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
		int get() const noexcept { return fd; }
		explicit operator bool() const noexcept { return fd != 1; }
		bool operator !() const noexcept { return fd == -1; }
		int release() {
			int r = fd;
			fd = -1;
			return r;
		}
		void reset(int newfd = -1) {
			close();
			fd = newfd;
		}
	private:
		void close() {
			if (*this) ::close(fd);
			fd = -1;
		}
		int fd;
	};

	struct unique_pipe {
		unique_fd r, w;
	};

	__attribute__((noreturn)) void throw_system_error(int err) {
		throw std::system_error(err, std::system_category());
	}

	template <typename T, typename D>
	std::unique_ptr<T, D> make_unique(T *p, const D &d) {
		return std::unique_ptr<T, D>(p, d);
	}

	std::string dirname(const std::string &path) {
		std::vector<char> buf(path.begin(), path.end());
		buf.push_back(0);
		return ::dirname(buf.data());
	}
	std::string basename(const std::string &path) {
		std::vector<char> buf(path.begin(), path.end());
		buf.push_back(0);
		return ::basename(buf.data());
	}
	std::string readlink(const std::string &path) {
		std::vector<char> buf(PATH_MAX);
		while (true) {
			const auto ret = ::readlink(path.c_str(), buf.data(), buf.size());
			if (ret < 0) throw_system_error(errno);
			if (static_cast<std::size_t>(ret) < buf.size()) return { buf.begin(), buf.begin()+ret };
			buf.resize(buf.size()*2);
		}
	}
	std::string realpath(const std::string &path) {
		const auto p = make_unique(::realpath(path.c_str(), nullptr), &::free);
		if (!p) throw_system_error(errno);
		return p.get();
	}
	std::string realpath_allowing_noent(const std::string &path) {
		const auto p = make_unique(::realpath(path.c_str(), nullptr), &::free);
		if (p) return p.get();
		const auto d = dirname(path) + "/";
		const auto q = make_unique(::realpath(d.c_str(), nullptr), &::free);
		if (!q) throw_system_error(errno);
		return d + q.get();
	}

	std::string getcwd() {
		const auto p = make_unique(::getcwd(nullptr, 0), &::free);
		return p.get();
	}

	void mkdir(const std::string &path, ::mode_t mode) {
		if (::mkdir(path.c_str(), mode) < 0) throw_system_error(errno);
	}

	std::shared_ptr<DIR> opendir(const std::string &path) {
		DIR *dir = ::opendir(path.c_str());
		if (!dir) throw_system_error(errno);
		return std::shared_ptr<DIR>(dir, &::closedir);
	}

	std::string mkdtemp(const std::string &base) {
		std::vector<char> buf(base.begin(), base.end());
		buf.push_back(0);
		if (!::mkdtemp(buf.data())) throw_system_error(errno);
		return buf.data();
	}

	void chdir(const std::shared_ptr<DIR> &workdir) {
		const int fd = ::dirfd(workdir.get());
		if (fd == -1) throw_system_error(errno);
		if (::fchdir(fd) == -1) throw_system_error(errno);
	}

	unique_pipe pipe() {
		int p[2];
		if (::pipe(p) < 0) throw_system_error(errno);
		return { unique_fd(p[0]), unique_fd(p[1]) };
	}

	void dup2(const unique_fd &src, int dst) {
		if (::dup2(src.get(), dst) < 0) throw_system_error(errno);
	}

	void dup2(const unique_fd &src, const unique_fd &dst) {
		if (::dup2(src.get(), dst.get()) < 0) throw_system_error(errno);
	}

	__attribute__((noreturn)) void execv(const std::vector<std::string> &argv) {
		std::vector<std::vector<char>> x;
		for (const auto &s: argv) x.emplace_back(s.c_str(), s.c_str()+s.length()+1);
		std::vector<char *> a;
		for (auto &s: x) a.emplace_back(s.data());
		a.push_back(nullptr);
		::execv(x.front().data(), a.data());
		throw_system_error(errno);
	}

	__attribute__((returns_twice)) pid_t fork() {
		int pid = ::fork();
		if (pid == -1) throw_system_error(errno);
		return pid;
	}

	pid_t waitpid(pid_t pid, int *status, int options) {
		pid_t ret = -1;
		do {
			ret = ::waitpid(pid, status, options);
			if (ret == -1 && errno != EINTR) throw_system_error(errno);
		} while (ret == -1);
		return ret;
	}

	struct child_process {
		intptr_t pid;
		unique_fd fd_stdin;
		unique_fd fd_stdout;
		unique_fd fd_stderr;
	};

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

	child_process piped_spawn(int mode, const std::shared_ptr<DIR> &workdir, const std::vector<std::string> &argv) {
		if (mode == _P_OVERLAY) {
			chdir(workdir);
			execv(argv);
		}
		auto pipe_stdin = pipe();
		auto pipe_stdout = pipe();
		auto pipe_stderr = pipe();
		if (const auto pid = fork()) {
			pipe_stdin.r.reset();
			pipe_stdout.w.reset();
			pipe_stderr.w.reset();

			child_process child { pid, std::move(pipe_stdin.w), std::move(pipe_stdout.r), std::move(pipe_stderr.r) };
			if (mode == _P_WAIT) {
				int st;
				waitpid(pid, &st, 0);
				child.pid = st;
				return child;
			}
			return child;
		} else try {
			chdir(workdir);
			dup2(pipe_stdin.r, 0);
			dup2(pipe_stdout.w, 1);
			dup2(pipe_stderr.w, 2);
			pipe_stdin.r.reset();
			pipe_stdin.w.reset();
			pipe_stdout.r.reset();
			pipe_stdout.w.reset();
			pipe_stderr.r.reset();
			pipe_stderr.w.reset();
			execv(argv);
		} catch (...) {
			std::terminate();
		}
	}

	struct unique_child_pid {
		explicit unique_child_pid(pid_t pid = 0): pid(pid), st(0), waited(false) { }
		unique_child_pid(const unique_child_pid &) = delete;
		unique_child_pid(unique_child_pid &&other): pid(0), st(0), waited(false) {
			std::swap(pid, other.pid);
			std::swap(st, other.st);
			std::swap(waited, other.waited);
		}
		unique_child_pid &operator =(const unique_child_pid &) = delete;
		unique_child_pid &operator =(unique_child_pid &&other) {
			std::swap(pid, other.pid);
			std::swap(st, other.st);
			std::swap(waited, other.waited);
			if (pid != other.pid) other.do_wait();
			other.pid = 0;
			other.st = 0;
			other.waited = false;
			return *this;
		}
		~unique_child_pid() {
			do_wait();
		}
		int wait() {
			return do_wait();
		}
		int wait_nonblock() {
			return do_wait(WNOHANG);
		}
		pid_t get() const noexcept { return pid; }
		bool finished() const noexcept { return waited; }
		bool empty() const noexcept { return pid == 0; }
	private:
		int do_wait(int flag = 0) {
			if (waited) return st;
			if (pid == 0) return 0;
			if (::waitpid(pid, &st, flag) <= 0) return 0;
			waited = true;
			return st;
		}
		pid_t pid;
		int st;
		bool waited;
	};

	std::shared_ptr<DIR> make_tmpdir(const std::string &seed) {
		while (true) try {
			return opendir(mkdtemp(seed));
		} catch (std::system_error &e) {
			if (e.code().value() != ENOTDIR) throw;
		}
	}

}
