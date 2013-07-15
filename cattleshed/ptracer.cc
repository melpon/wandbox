#include <iostream>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <vector>
#include <string>
#include <algorithm>
#include <map>

#include <boost/range.hpp>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/numeric.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_match.hpp>
#include <boost/lexical_cast.hpp>

#include <cstddef>
#include <cstdint>

#include <cerrno>
#include <csignal>
#include <cstring>

#include <unistd.h>
#include <fcntl.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <sys/syscall.h>
#include <sys/reg.h>
#include <syslog.h>

#ifndef PTRACE_O_EXITKILL
#define PTRACE_O_EXITKILL (1 << 20)
#undef  PTRACE_O_MASK
#define PTRACE_O_MASK     (0x000000ff | PTRACE_O_EXITKILL)
#endif

#define read_reg(pid, name) ptrace(PTRACE_PEEKUSER, pid, offsetof(user_regs_struct, name), 0)
#define write_reg(pid, name, val) ptrace(PTRACE_POKEUSER, pid, offsetof(user_regs_struct, name), val)

namespace wandbox {
	template <typename T, bool ...>
	struct do_to_string_impl;
	template <typename T, bool ...B>
	struct do_to_string_impl<T, true, B...> {
		// use implicit conversion
		static std::string call(const T &x) { return x; }
	};
	template <typename T, bool ...B>
	struct do_to_string_impl<T, false, true, B...> {
		// use std::to_string
		static std::string call(const T &x) { return std::to_string(x); }
	};
	template <typename T, bool ...B>
	struct do_to_string_impl<T, false, false, true, B...> {
		// use boost::lexical_cast
		static std::string call(const T &x) { return boost::lexical_cast<std::string>(x); }
	};
	template <typename T>
	struct do_to_string {
		template <typename U, typename = decltype(std::to_string(std::declval<U>()))>
		static std::true_type check_std_to_string(U *);
		static std::false_type check_std_to_string(void *);
		typedef decltype(check_std_to_string((T*)0)) use_std_to_string;
		template <typename U, typename = decltype(boost::lexical_cast(std::declval<U>()))>
		static std::true_type check_lexical_cast(U *);
		static std::false_type check_lexical_cast(void *);
		typedef decltype(check_lexical_cast((T*)0)) use_lexical_cast;
		static std::string call(const T &x) {
			return do_to_string_impl<
				T,
				std::is_convertible<T, std::string>::type::value,
				boost::mpl::identity<decltype(check_std_to_string((T*)0))>::type::value,
				boost::mpl::identity<decltype(check_lexical_cast((T*)0))>::type::value
			>::call(x);
		}
	};
	template <typename ...Args>
	void trace(int pri, const char *const fmt, const Args &...args) {
		syslog(pri, fmt, do_to_string<Args>::call(args).c_str()...);
	}
	namespace rng {
		using namespace boost::range;
		using namespace boost::adaptors;
		using boost::accumulate;
		template <typename SinglePassRange>
		typename boost::range_value<SinglePassRange>::type accumulate1(const SinglePassRange &src) {
			if (boost::empty(src)) throw std::runtime_error("empty range");
			auto ite = boost::begin(src);
			const auto &first = *ite++;
			return std::accumulate(ite, boost::end(src), first);
		}
		template <typename SinglePassRange, typename BinaryOp>
		typename boost::range_value<SinglePassRange>::type accumulate1(const SinglePassRange &src, const BinaryOp &op) {
			if (boost::empty(src)) throw std::runtime_error("empty range");
			auto ite = boost::begin(src);
			const auto &first = *ite++;
			return std::accumulate(ite, boost::end(src), first, op);
		}
	}
	template <typename AddrT, typename DataT>
	inline long ptrace(__ptrace_request req, pid_t pid, AddrT addr, DataT data) {
		return ::ptrace(req, pid, (void *)(size_t)addr, (void *)(size_t)data);
	}
	std::string read_cstring_from_process(pid_t pid, uintptr_t addr) {
		std::vector<char> buf;
		while (std::find(buf.begin(), buf.end(), 0) == buf.end()) {
			const long d = ptrace(PTRACE_PEEKDATA, pid, addr, 0);
			for (unsigned n = 0; n < sizeof(d); ++n) buf.push_back(d>>(n*8));
			addr += sizeof(d);
		}
		return buf.data();
	}
	/// パスコンポーネントを正規化する(./ と ../ を削除する)
	/// @param src 入力パス(絶対パスのみ)
	/// @return 正規化パスまたは空文字列
	std::string canonicalize_path(const std::string &src) {
		// realpath(3) は symlink を解決してしまう
		// boost::filesystem::canonical はファイルが存在しないと失敗する
		// ので、/proc/self の場合どちらも役に立たない
		if (src.empty() || src[0] != '/') return "";
		const bool absolutep = src[0] == '/';

		std::vector<std::string> dirs;
		auto ite = src.begin();
		if (absolutep) ++ite;
		const auto end = src.end();
		namespace qi = boost::spirit::qi;
		qi::parse(ite, end, *(qi::char_ - '/') % '/', dirs);
		return (absolutep ? "/" : "") + rng::accumulate1(
			rng::accumulate(
				dirs | rng::filtered([](const std::string &s) { return !s.empty() && s != "."; }),
				std::vector<std::string>(),
				[absolutep](std::vector<std::string> a, std::string s) -> std::vector<std::string> {
					if (s == "..") {
						if (absolutep) {
							if (!a.empty()) a.pop_back();
						} else {
							if (a.empty() || a.back() == "..") a.push_back("..");
							else a.pop_back();
						}
					} else {
						a.emplace_back(move(s));
					}
					return move(a);
				}),
			[](const std::string &a, const std::string &b) { return a + "/" + b; });
	}
	inline bool starts_with(const std::string &tested, const std::string &prefix) {
		return tested.length() >= prefix.length() && tested.substr(0, prefix.length()) == prefix;
	}
	/// ファイルを開かせてもよいかどうか判断する
	/// @return 開かせてよい時 @true
	bool is_file_openable(const std::string &path, int flags, mode_t) {
		// TODO: この辺の許可リストは外から読むようにしたい
		static const auto allowed_paths = []() -> std::vector<std::string> {
			std::vector<std::string> l = {"/etc/ld.so.cache", "/dev/null", "/dev/zero", "/proc/stat"};
			std::sort(l.begin(), l.end());
			return l;
		}();
		static const std::vector<std::string> allowed_prefixes = { "/lib", "/usr/lib", "/proc/self" };
		const auto realpath = canonicalize_path(path);
		const int openmode = flags & O_ACCMODE;
		if (rng::binary_search(allowed_paths, path) && openmode == O_RDONLY) return true;
		if (rng::find_if(allowed_prefixes, std::bind(starts_with, realpath, std::placeholders::_1)) != allowed_prefixes.end() && openmode == O_RDONLY) return true;
		if (!starts_with(realpath, "..") && !starts_with(realpath, "/")) return true;
		trace(LOG_DEBUG, "opening path '%s' is not allowed", path);
		return false;
	}
	/// ファイルを stat してもよいかどうか判断する
	/// @return 開かせてよい時 @true
	bool is_file_stattable(const std::string &path) {
		// FIXME: 上と完全に dup しているのでどうにかする
		static const auto allowed_paths = []() -> std::vector<std::string> {
			std::vector<std::string> l = {"/etc/ld.so.cache", "/dev/null", "/dev/zero", "/proc/stat"};
			std::sort(l.begin(), l.end());
			return l;
		}();
		static const std::vector<std::string> allowed_prefixes = { "/lib", "/usr/lib", "/proc/self" };
		const auto realpath = canonicalize_path(path);
		if (rng::binary_search(allowed_paths, path)) return true;
		if (rng::find_if(allowed_prefixes, std::bind(starts_with, realpath, std::placeholders::_1)) != allowed_prefixes.end()) return true;
		if (!starts_with(realpath, "..") && !starts_with(realpath, "/")) return true;
		trace(LOG_DEBUG, "statting path '%s' is not allowed", path);
		return false;
	}
	/// システムコールの呼出しを許すかどうか判断する
	/// @return 呼び出しを許す時 @true
	bool is_permitted_syscall(pid_t pid, user_regs_struct &reg) {
		switch (reg.orig_rax) {
			// ここにずらーーーーーーっと許可syscallを並べる
			// Aは場合によってはblockしたい
			// Bは場合によっては許可したい
		case SYS_open: // B
			return is_file_openable(read_cstring_from_process(pid, reg.rdi), reg.rsi, reg.rdx);

		case SYS_stat: // B
			return is_file_stattable(read_cstring_from_process(pid, reg.rdi));

		case SYS_clone: // B
			return (read_reg(pid, rdi) & CLONE_THREAD) != 0;

		case SYS_read: // B
		case SYS_write: // B

		case SYS_close:
		case SYS_fstat:
		case SYS_lstat:
		case SYS_poll:

		case SYS_lseek:
		case SYS_mmap:
		case SYS_mprotect: // A
		case SYS_munmap:
		case SYS_brk:
		case SYS_rt_sigaction:
		case SYS_rt_sigprocmask:
		case SYS_rt_sigreturn:

		case SYS_pread64:
		case SYS_pwrite64:
		case SYS_readv:
		case SYS_writev:
		case SYS_access:
		case SYS_pipe:
		case SYS_select:

		case SYS_sched_yield:
		case SYS_mremap:
		case SYS_mincore:
		case SYS_madvise:

		case SYS_pause:
		case SYS_nanosleep:
		case SYS_getitimer:
		case SYS_alarm:
		case SYS_setitimer:
		case SYS_getpid:

		case SYS_exit:
		case SYS_uname:

		case SYS_gettimeofday:
		case SYS_getrlimit:
		case SYS_getrusage:
		case SYS_sysinfo:
		case SYS_times:

		case SYS_rt_sigpending:
		case SYS_rt_sigtimedwait:
		case SYS_rt_sigqueueinfo:
		case SYS_rt_sigsuspend:
		case SYS_sigaltstack:

		case SYS_arch_prctl:

		case SYS_clock_gettime:
		case SYS_clock_nanosleep:
		case SYS_exit_group:

		case SYS_set_robust_list:
		case SYS_set_tid_address:
		case SYS_timer_create:
		case SYS_timer_delete:
		case SYS_timer_settime:

		case SYS_fcntl:
		case SYS_futex:
		case SYS_getcwd:
		case SYS_sched_getaffinity:
		case SYS_tgkill:
			return true;
		}
		return false;
	}
	enum struct run_state_t {
		running,
		syscall_executing,
		syscall_refused,
	};
	int trace_loop(const pid_t primary_pid, const sigset_t sigs) {
		int exitcode = 0;
		std::map<pid_t, run_state_t> states;
		while (1) {
			{
				siginfo_t siginfo;
				const int sig = sigwaitinfo(&sigs, &siginfo);
				if (sig == -1) return -1;
				if (sig != SIGCHLD) {
					trace(LOG_DEBUG, "forwarding signal %s %s", primary_pid, siginfo.si_signo);
					ptrace(PTRACE_SYSCALL, primary_pid, 0, siginfo.si_signo);
					continue;
				}
			}

			while (1) {
				int status = 0;
				errno = 0;
				const int pid = ::waitpid(-1, &status, __WALL | WNOHANG);
				if (pid == 0) break;
				if (pid == -1) {
					if (errno == ECHILD) return exitcode;
					return -1;
				}
				if (WIFEXITED(status)) {
					if (pid == primary_pid) exitcode = WEXITSTATUS(status);
					continue;
				}
				if (WIFSIGNALED(status)) {
					if (pid == primary_pid) exitcode = -WTERMSIG(status);
					continue;
				}
				if (WIFCONTINUED(status)) {
					ptrace(PTRACE_SYSCALL, pid, 0, 0);
					continue;
				}
				const int stopsig = WSTOPSIG(status);

				if (stopsig != (SIGTRAP|0x80)) {
					unsigned long newpid = 0;
					if (((status>>16) & (PTRACE_EVENT_CLONE|PTRACE_EVENT_FORK|PTRACE_EVENT_VFORK)) != 0) {
						ptrace(PTRACE_GETEVENTMSG, pid, 0, &newpid);
					}
					if (newpid != 0) {
						trace(LOG_DEBUG, "[%s]cloned to %s", pid, newpid);
						ptrace(PTRACE_SETOPTIONS, newpid, 0, PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK | PTRACE_O_EXITKILL);
						ptrace(PTRACE_SYSCALL, newpid, 0, 0);
						ptrace(PTRACE_SYSCALL, pid, 0, 0);
					} else {
						const int realsig = stopsig & ~0x80;
						trace(LOG_DEBUG, "[%s]child signaled: %s", pid, realsig);
						ptrace(PTRACE_SYSCALL, pid, 0, realsig);
					}
					continue;
				}

				user_regs_struct reg;
				ptrace(PTRACE_GETREGS, pid, 0, &reg);
				switch (states[pid]) {
				case run_state_t::running:
					{
						const bool refused = !is_permitted_syscall(pid, reg);
						trace(LOG_DEBUG, "[%s]enter syscall %s%s(%s, %s, %s, %s, %s, %s)", pid, refused?"[blocked] ":"", reg.orig_rax, reg.rdi, reg.rsi, reg.rdx, reg.r10, reg.r8, reg.r9);
						if (refused) {
							write_reg(pid, orig_rax, -1);
							states[pid] = run_state_t::syscall_refused;
						} else {
							states[pid] = run_state_t::syscall_executing;
						}
					}
					break;
				case run_state_t::syscall_executing:
					trace(LOG_DEBUG, "[%s]leave syscall %s (%s)", pid, reg.orig_rax, reg.rax);
					states[pid] = run_state_t::running;
					break;
				case run_state_t::syscall_refused:
					trace(LOG_DEBUG, "[%s]leave syscall [blocked] %s (%s)", pid, reg.orig_rax, reg.rax);
					write_reg(pid, rax, -EPERM);
					states[pid] = run_state_t::running;
					break;
				}
				ptrace(PTRACE_SYSCALL, pid, 0, 0);
			}
		}
		return 0;
	}

	bool kernel_has_ptrace_o_exitkill() {
		namespace qi = boost::spirit::qi;
		std::ifstream ifs("/proc/version");
		int major = 0, minor = 0;
		ifs >> qi::match(qi::lit("Linux") >> qi::lit("version") >> qi::int_ >> '.' >> qi::int_, major, minor);
		return major > 3 || (major == 3 && minor >= 8);
	}

	int main(int argc, char **argv) {
		if (argc < 2) return -1;

		if (not kernel_has_ptrace_o_exitkill()) return -1;

		sigset_t sigs;
		sigfillset(&sigs);
		sigdelset(&sigs, SIGKILL);
		sigdelset(&sigs, SIGSTOP);
		sigprocmask(SIG_BLOCK, &sigs, 0);
		pid_t pid = ::fork();
		if (pid == 0) {
			sigprocmask(SIG_UNBLOCK, &sigs, 0);
			ptrace(PTRACE_TRACEME, 0, 0, 0);
			++argv;
			return ::execv(*argv, argv);
		} else if (pid > 0) {
			::openlog("ptracer", LOG_PID|LOG_CONS, LOG_AUTHPRIV);
			::waitpid(pid, 0, 0);
			ptrace(PTRACE_SETOPTIONS, pid, 0, PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK | PTRACE_O_EXITKILL);
			ptrace(PTRACE_SYSCALL, pid, 0, 0);
			return trace_loop(pid, sigs);
		} else {
			return -1;
		}
	}
}

int main(int argc, char **argv) {
	return wandbox::main(argc, argv);
}
