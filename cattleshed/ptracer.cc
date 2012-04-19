#include <iostream>
#include <iomanip>

#include <unistd.h>
#include <cstddef>
#include <errno.h>
#include <signal.h>
#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/user.h>
#include <sys/syscall.h>
#include <sys/reg.h>

#define PTRACE_SYSEMU ((enum __ptrace_request)31)


#define read_reg(pid, name) ::ptrace(PTRACE_PEEKUSER, pid, offsetof(user_regs_struct, name), 0)
#define write_reg(pid, name, val) ::ptrace(PTRACE_POKEUSER, pid, offsetof(user_regs_struct, name), val)

namespace wandbox {
	int main(int argc, char **argv) {
		if (argc < 2) return -1;

		sigset_t sigs;
		sigfillset(&sigs);
		sigdelset(&sigs, SIGKILL);
		sigdelset(&sigs, SIGSTOP);
		sigprocmask(SIG_BLOCK, &sigs, 0);
		pid_t pid = ::fork();
		if (pid == 0) {
			sigprocmask(SIG_UNBLOCK, &sigs, 0);
			::ptrace(PTRACE_TRACEME, 0, 0, 0);
			++argv;
			return ::execv(*argv, argv);
		} else if (pid > 0) {
			bool in_syscall = false;
			bool noperm = false;
			//std::cout << std::hex;
			::waitpid(pid, 0, 0);
			::ptrace(PTRACE_SETOPTIONS, pid, 0, PTRACE_O_TRACESYSGOOD);
			int st;
			int sig = 0;
			while (1) {

				if (sig >= 0) ::ptrace(PTRACE_SYSCALL, pid, 0, sig);
				if (sigwait(&sigs, &sig)) {
					sig = -1;
					continue;
				} else if (sig != SIGCHLD) {
					kill(pid, sig);
					continue;
				}

				user_regs_struct reg;
				switch (::waitpid(pid, &st, WNOHANG)) {
				case -1: return -1;
				case 0: sig = -1; continue;
				default: sig = 0; break;
				}

				if (WIFEXITED(st) || WIFSIGNALED(st)) return st;
				//std::cout << "signo = " << WSTOPSIG(st) << std::endl;
				if (WSTOPSIG(st) != (SIGTRAP|0x80)) {
					sig = WSTOPSIG(st) & ~0x80;
					continue;
				}
				::ptrace(PTRACE_GETREGS, pid, 0, &reg);
				if (in_syscall) {
					//std::cout << "leave syscall " << reg.orig_rax << '(' << reg.rax << ')' << std::endl;
					if (noperm) write_reg(pid, rax, 0xFFFFFFFFFFFFFFFF);
					noperm = false;
				} else {
					switch (reg.orig_rax) {
						// ここにずらーーーーーーっと許可syscallを並べる
						// Aは場合によってはblockしたい
						// Bは場合によっては許可したい
					case SYS_read: // B
					case SYS_write: // B
					case SYS_open: // B
					case SYS_close:
					case SYS_stat:
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

					case SYS_execve:
					case SYS_ioctl:
					case SYS_set_robust_list:
					case SYS_set_tid_address:
					case SYS_timer_create:
					case SYS_timer_delete:
					case SYS_timer_settime:
						noperm = false;
						break;
					case SYS_clone: // B
					default:
						noperm = true;
					}
					//std::cout << "enter syscall " << (noperm ? "[blocked]" : "") << reg.orig_rax << '(' << reg.rdi << ", " << reg.rsi << ", " << reg.rdx << ", " << reg.r10 << ", " << reg.r8 << ", " << reg.r9 << ')' << std::endl;
					if (noperm) write_reg(pid, orig_rax, -1);
				}
				in_syscall = !in_syscall;
			}
			return st;
		} else {
			return -1;
		}
	}
}

int main(int argc, char **argv) {
	return wandbox::main(argc, argv);
}
