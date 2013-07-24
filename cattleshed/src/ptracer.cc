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
#include <getopt.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <sys/reg.h>
#include <sys/resource.h>
#include <syslog.h>

#include "load_config.hpp"
#include "posixapi.hpp"

#ifndef PTRACE_O_EXITKILL
#define PTRACE_O_EXITKILL (1 << 20)
#undef  PTRACE_O_MASK
#define PTRACE_O_MASK     (0x000000ff | PTRACE_O_EXITKILL)
#endif

#define read_reg(pid, name) ptrace(PTRACE_PEEKUSER, pid, offsetof(user_regs_struct, name), 0)
#define write_reg(pid, name, val) ptrace(PTRACE_POKEUSER, pid, offsetof(user_regs_struct, name), val)

namespace wandbox {

	server_config config;

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
	bool is_file_prefixed_by(const std::string &path, const std::string &prefix) {
		return path.length() == prefix.length() 
			? path == prefix
			: prefix.length() < path.length() && std::equal(prefix.begin(), prefix.end(), path.begin()) && path[prefix.length()] == '/';
	}
	enum struct allowed_access_mode {
		none,
		readonly,
		readwrite
	};
	/// path に対して許可されたアクセスモードを判定する
	/// $PWD に対する読み書きアクセスと, config で列挙された対象または /proc/self/* に対する読み込みアクセスが可能
	allowed_access_mode get_file_accessiblity(pid_t pid, const std::string &path) try {
		const auto r = realpath_allowing_noent(path);
		const auto procself = realpath("/proc/" + std::to_string(pid));
		const auto is_prefixing = [&](const std::string &prefix) { return is_file_prefixed_by(r, prefix); };
		if (is_prefixing(getcwd())) return allowed_access_mode::readwrite;
		if (config.jail.allow_file_exact.count(r) == 1) return allowed_access_mode::readonly;
		if (rng::find_if(config.jail.allow_file_prefix, is_prefixing) != config.jail.allow_file_prefix.end()) return allowed_access_mode::readonly;
		if (is_prefixing(realpath(procself + "/exe"))) return allowed_access_mode::readonly;
		return allowed_access_mode::none;
	} catch (std::system_error &) {
		return allowed_access_mode::none;
	}

	/// ファイルを開かせてもよいかどうか判断する
	/// @return 開かせてよい時 @true
	bool is_file_openable(pid_t pid, const std::string &path, int flags, mode_t) {
		const int openmode = flags & O_ACCMODE;
		switch (get_file_accessiblity(pid, path)) {
		case allowed_access_mode::readonly:
			if (openmode != O_RDONLY) trace(LOG_DEBUG, "opening to write path '%s' is not allowed", path);
			return openmode == O_RDONLY;
		case allowed_access_mode::readwrite:
			return true;
		default:
			trace(LOG_DEBUG, "opening path '%s' is not allowed", path);
		}
		return false;
	}
	/// ファイルを stat してもよいかどうか判断する
	/// @return 開かせてよい時 @true
	bool is_file_stattable(pid_t pid, const std::string &path) {
		switch (get_file_accessiblity(pid, path)) {
		case allowed_access_mode::readonly:
		case allowed_access_mode::readwrite:
			return true;
		default:
			trace(LOG_DEBUG, "statting path '%s' is not allowed", path);
		}
		return false;
	}
	bool is_acceptable_clone_flag(unsigned long flag) {
		return (flag & CLONE_THREAD) != 0
			&& (flag & CLONE_UNTRACED) == 0;
	}
	/// システムコールの呼出しを許すかどうか判断する
	/// @return 呼び出しを許す時 @true
	bool is_permitted_syscall(pid_t pid, user_regs_struct &reg) {
		switch (reg.orig_rax) {
			// ここにずらーーーーーーっと許可syscallを並べる
			// Aは場合によってはblockしたい
			// Bは場合によっては許可したい
		case SYS_open: // B
			return is_file_openable(pid, read_cstring_from_process(pid, reg.rdi), reg.rsi, reg.rdx);

		case SYS_stat: // B
			return is_file_stattable(pid, read_cstring_from_process(pid, reg.rdi));

		case SYS_clone: // B
			return is_acceptable_clone_flag(read_reg(pid, rdi));

		case SYS_execve:
		{
			static bool first_exec_already_called = false;
			if (first_exec_already_called) return false;
			first_exec_already_called = true;
			return true;
		}

		case SYS_read: // B
		case SYS_write: // B

		case SYS_close:
		case SYS_fstat:
		case SYS_lstat:
		case SYS_readlink:
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
		case SYS_pipe2:
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
		int laststatus = 0;
		std::map<pid_t, run_state_t> states;
		while (1) {
			{
				siginfo_t siginfo;
				const int sig = sigwaitinfo(&sigs, &siginfo);
				if (sig == -1) return -1;
				if (sig != SIGCHLD) {
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
					if (errno == ECHILD) return laststatus;
					return -1;
				}
				if (WIFEXITED(status) || WIFSIGNALED(status)) {
					if (pid == primary_pid) laststatus = status;
					continue;
				}
				if (WIFCONTINUED(status)) {
					ptrace(PTRACE_SYSCALL, pid, 0, 0);
					continue;
				}
				const int stopsig = WSTOPSIG(status);

				if (stopsig != (SIGTRAP|0x80)) {
					unsigned long newpid = 0;
					if (((status>>16) & PTRACE_EVENT_EXEC) != 0) {
						ptrace(PTRACE_SYSCALL, pid, 0, 0);
						continue;
					}
					if (((status>>16) & (PTRACE_EVENT_CLONE|PTRACE_EVENT_FORK|PTRACE_EVENT_VFORK)) != 0) {
						ptrace(PTRACE_GETEVENTMSG, pid, 0, &newpid);
					}
					if (newpid != 0) {
						trace(LOG_DEBUG, "[%s] cloned to %s", pid, newpid);
						ptrace(PTRACE_SETOPTIONS, newpid, 0, PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK | PTRACE_O_TRACEEXEC | PTRACE_O_EXITKILL);
						ptrace(PTRACE_SYSCALL, newpid, 0, 0);
						ptrace(PTRACE_SYSCALL, pid, 0, 0);
					} else {
						siginfo_t realsig;
						ptrace(PTRACE_GETSIGINFO, pid, 0, &realsig);
						ptrace(PTRACE_SYSCALL, pid, 0, realsig.si_signo);
					}
					continue;
				}

				user_regs_struct reg;
				ptrace(PTRACE_GETREGS, pid, 0, &reg);
				switch (states[pid]) {
				case run_state_t::running:
					{
						const bool refused = !is_permitted_syscall(pid, reg);
						if (refused) {
							trace(LOG_DEBUG, "[%s] blocked syscall %s (%s, %s, %s, %s, %s, %s)", pid, reg.orig_rax, reg.rdi, reg.rsi, reg.rdx, reg.r10, reg.r8, reg.r9);
						}
						if (refused) {
							write_reg(pid, orig_rax, -1);
							states[pid] = run_state_t::syscall_refused;
						} else {
							states[pid] = run_state_t::syscall_executing;
						}
					}
					break;
				case run_state_t::syscall_executing:
					states[pid] = run_state_t::running;
					break;
				case run_state_t::syscall_refused:
					write_reg(pid, rax, -EPERM);
					states[pid] = run_state_t::running;
					break;
				}
				ptrace(PTRACE_SYSCALL, pid, 0, 0);
			}
		}
		return 0;
	}

	void apply_system_resource_limits() {
		const auto setrlimit = [](int resource, ::rlim_t lim) {
			const ::rlimit xlim = { lim, lim };
			::setrlimit(resource, &xlim);
		};
		setrlimit(RLIMIT_AS, config.jail.max_address_space);
		setrlimit(RLIMIT_CPU, config.jail.max_cpu_time);
		setrlimit(RLIMIT_DATA, config.jail.max_data_segment);
		setrlimit(RLIMIT_FSIZE, config.jail.max_file_size);
		setrlimit(RLIMIT_NOFILE, config.jail.max_open_file);
		if (::nice(config.jail.nice) < 0) throw std::runtime_error("nice(2) failed");
	}

	bool kernel_has_ptrace_o_exitkill() {
		namespace qi = boost::spirit::qi;
		std::ifstream ifs("/proc/version");
		int major = 0, minor = 0;
		ifs >> qi::match(qi::lit("Linux") >> qi::lit("version") >> qi::int_ >> '.' >> qi::int_, major, minor);
		return major > 3 || (major == 3 && minor >= 8);
	}

	int do_fork_exec(char **argv) {
		sigset_t sigs;
		sigfillset(&sigs);
		sigdelset(&sigs, SIGKILL);
		sigdelset(&sigs, SIGSTOP);
		sigprocmask(SIG_BLOCK, &sigs, 0);
		pid_t pid = ::fork();
		if (pid == 0) {
			apply_system_resource_limits();
			sigprocmask(SIG_UNBLOCK, &sigs, 0);
			ptrace(PTRACE_TRACEME, 0, 0, 0);
			::raise(SIGSTOP);
			::exit(::execv(*argv, argv));
		} else if (pid > 0) {
			::openlog("ptracer", LOG_PID|LOG_CONS, LOG_AUTHPRIV);
			::waitpid(pid, 0, 0);
			ptrace(PTRACE_SETOPTIONS, pid, 0, PTRACE_O_TRACESYSGOOD | PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK | PTRACE_O_TRACEEXEC | PTRACE_O_EXITKILL);
			ptrace(PTRACE_SYSCALL, pid, 0, 0);
			int status = trace_loop(pid, sigs);
			if (WIFSIGNALED(status)) {
				const int sig = WTERMSIG(status);
				sigemptyset(&sigs);
				sigaddset(&sigs, sig);
				sigprocmask(SIG_UNBLOCK, &sigs, 0);
				raise(sig);
			}
			if (WIFEXITED(status)) {
				return WEXITSTATUS(status);
			}
			return -1;
		} else {
			return -1;
		}
	}

	int main(int argc, char **argv) {
		if (not kernel_has_ptrace_o_exitkill()) return -1;

		std::string config_file = std::string(DATADIR) + "/config";

		{
			const ::option opts[] = {
				{ "help", no_argument, nullptr, 'h' },
				{ "config", required_argument, nullptr, 'c' },
				{ },
			};
			while (true) {
				const int c = ::getopt_long(argc, argv, "hc:", opts, nullptr);
				switch (c) {
				case -1: goto end_getopt;
				case 'c':
					config_file = ::optarg;
					break;
				case 'h':
					std::cout << "usage: " << argv[0] << " {-c configfile} -- commands..." << std::endl;
					return 0;
				case '?':
					std::cerr << "unknown option '" << (char)optopt << "'" << std::endl;
					return -1;
				default:
					std::cerr << "unknown option '" << (char)c << "'" << std::endl;
					return -1;
				}
			}
		end_getopt:;
		} {
			const std::shared_ptr<char> p(::realpath(config_file.c_str(), nullptr), &::free);
			if (!p) {
				std::cerr << "config file '" << config_file << "'not found" << std::endl;
				return -1;
			}
			config_file = p.get();
		} {
			std::ifstream f(config_file.c_str());
			config = load_config(f);
		}

		return do_fork_exec(argv+optind);
	}
}

int main(int argc, char **argv) {
	return wandbox::main(argc, argv);
}
