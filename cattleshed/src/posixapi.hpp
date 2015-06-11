#ifndef POSIXAPI_HPP_
#define POSIXAPI_HPP_

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

	__attribute__((noreturn)) inline void throw_system_error(int err) {
		throw std::system_error(err, std::system_category());
	}

	template <typename T, typename D>
	inline std::unique_ptr<T, D> make_unique(T *p, const D &d) {
		return std::unique_ptr<T, D>(p, d);
	}

	inline std::string realpath(const std::string &path) {
		const auto p = make_unique(::realpath(path.c_str(), nullptr), &::free);
		if (!p) throw_system_error(errno);
		return p.get();
	}

	inline int dirfd_or_cwd(const std::shared_ptr<DIR> &dir) {
		if (dir) return ::dirfd(dir.get());
		return AT_FDCWD;
	}

	inline void mkdir(const std::string &path, ::mode_t mode) {
		if (::mkdir(path.c_str(), mode) < 0) throw_system_error(errno);
	}

	inline void mkdirat(const std::shared_ptr<DIR> &at, const std::string &path, ::mode_t mode) {
		if (::mkdirat(dirfd_or_cwd(at), path.c_str(), mode) < 0) throw_system_error(errno);
	}

	inline std::shared_ptr<DIR> opendir(const std::string &path) {
		DIR *dir = ::opendir(path.c_str());
		if (!dir) throw_system_error(errno);
		return std::shared_ptr<DIR>(dir, &::closedir);
	}

	inline std::shared_ptr<DIR> opendirat(const std::shared_ptr<DIR> &at, const std::string &path) {
		int fd = ::openat(dirfd_or_cwd(at), path.c_str(), O_RDONLY|O_DIRECTORY);
		if (fd == -1) throw_system_error(errno);
		return std::shared_ptr<DIR>(fdopendir(fd), &::closedir);
	}

	inline std::vector<std::string> split_path_impl(const std::string &path, bool tree) {
		std::vector<std::string> ret;
		auto sep = path.begin();
		const auto end = path.end();
		for (auto ite = path.begin(); ite != end; ++ite) {
			if (*ite == '/') {
				std::string s(sep, ite);
				if (!s.empty() && s != ".") {
					if (s == "..") {
						if (!ret.empty()) ret.pop_back();
					} else {
						if (tree && !ret.empty()) {
							ret.emplace_back(ret.back() + "/" + s);
						} else {
							ret.emplace_back(std::move(s));
						}
					}
				}
				sep = ite;
				++sep;
				continue;
			}
			if (*ite == 0) throw_system_error(errno = ENOENT);
		}
		if (sep != end) ret.emplace_back(sep, end);
		return ret;
	}

	inline std::vector<std::string> split_path(const std::string &path) {
		return split_path_impl(path, false);
	}

	inline std::vector<std::string> split_path_tree(const std::string &path) {
		return split_path_impl(path, true);
	}

	inline std::shared_ptr<DIR> mkdir_p_open_at(const std::shared_ptr<DIR> &at, const std::string &path, ::mode_t mode) {
		// FIXME: must canonicalize invalid UTF-8 sequence in `path'
		if (path.empty()) throw_system_error(errno = ENOENT);

		std::vector<std::shared_ptr<DIR> > dirfds;
		const auto dirnames = split_path(path);

		dirfds.reserve(dirnames.size()+1);
		if (path[0] == '/') dirfds.emplace_back(opendir("/"));
		else dirfds.push_back(at);

		for (auto &&x: dirnames) {
			if (x == "") continue;
			if (x == ".") continue;
			if (x == "..") {
				if (dirfds.empty()) return at;
				dirfds.pop_back();
			} else {
				try {
					mkdirat(dirfds.back(), x, mode);
				} catch (std::system_error &e) {
					if (e.code().value() != EEXIST) throw;
				}
				dirfds.push_back(opendirat(dirfds.back(), x));
			}
		}

		return move(dirfds.back());
	}

	inline std::string mkdtemp(const std::string &base) {
		std::vector<char> buf(base.begin(), base.end());
		buf.push_back(0);
		if (!::mkdtemp(buf.data())) throw_system_error(errno);
		return buf.data();
	}

	inline void chdir(const std::shared_ptr<DIR> &workdir) {
		const int fd = ::dirfd(workdir.get());
		if (fd == -1) throw_system_error(errno);
		if (::fchdir(fd) == -1) throw_system_error(errno);
	}

	inline unique_pipe pipe() {
		int p[2];
		if (::pipe(p) < 0) throw_system_error(errno);
		return { unique_fd(p[0]), unique_fd(p[1]) };
	}

	inline void dup2(const unique_fd &src, int dst) {
		if (::dup2(src.get(), dst) < 0) throw_system_error(errno);
	}

	inline void dup2(const unique_fd &src, const unique_fd &dst) {
		if (::dup2(src.get(), dst.get()) < 0) throw_system_error(errno);
	}

	__attribute__((noreturn)) inline void execv(const std::vector<std::string> &argv) {
		std::vector<std::vector<char>> x;
		for (const auto &s: argv) x.emplace_back(s.c_str(), s.c_str()+s.length()+1);
		std::vector<char *> a;
		for (auto &s: x) a.emplace_back(s.data());
		a.push_back(nullptr);
		::execv(x.front().data(), a.data());
		throw_system_error(errno);
	}

	__attribute__((returns_twice)) inline pid_t fork() {
		int pid = ::fork();
		if (pid == -1) throw_system_error(errno);
		return pid;
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

	struct child_process {
		unique_child_pid pid;
		unique_fd fd_stdin;
		unique_fd fd_stdout;
		unique_fd fd_stderr;
	};

	inline child_process piped_spawn(const std::shared_ptr<DIR> &workdir, const std::vector<std::string> &argv) {
		auto pipe_stdin = pipe();
		auto pipe_stdout = pipe();
		auto pipe_stderr = pipe();
		if (const auto pid = fork()) {
			return { unique_child_pid(pid), std::move(pipe_stdin.w), std::move(pipe_stdout.r), std::move(pipe_stderr.r) };
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
}
#endif
