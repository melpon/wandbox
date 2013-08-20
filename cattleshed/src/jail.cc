#include <string>
#include <functional>
#include <iostream>

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include <boost/asio.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/optional.hpp>

#include <dirent.h>
#include <fcntl.h>
#include <getopt.h>
#include <grp.h>
#include <linux/securebits.h>
#include <pwd.h>
#include <sched.h>
#include <sys/capability.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

namespace wandbox {
namespace jail {
	int wait_and_forward_signals(int primary_child_pid) {
		struct sigaction actions[256];
		for (int n = 0; n < 256; ++n) sigaction(n, nullptr, &actions[n]);
		namespace asio = boost::asio;
		asio::io_service aio;
		asio::signal_set sigs(aio);
		{
			boost::system::error_code ec;
			for (int n = 0; n < 256; ++n) sigs.add(n, ec);
		}
		int ret = 0;
		std::function<void(boost::system::error_code, int)> f;
		f = [&](boost::system::error_code ec, int sig) {
			if (ec) return;
			int st_;
			bool waited = false;
			while (true) {
				const int r = waitpid(-1, &st_, WNOHANG|__WALL);
				if (r == -1) return;
				if (r == 0 && !waited) { kill(primary_child_pid, sig); break; }
				if (r == primary_child_pid) ret = st_;
				waited = true;
			}
			sigs.async_wait(f);
		};
		sigs.async_wait(f);
		{
			sigset_t sigs;
			sigfillset(&sigs);
			sigprocmask(SIG_UNBLOCK, &sigs, nullptr);
		}
		aio.run();
		for (int n = 0; n < 256; ++n) sigaction(n, &actions[n], nullptr);
		return ret;
	}
	struct proc_arg_t {
		std::vector<std::string> mount_dirs;
		std::vector<std::string> devices;
		std::string fakehome;
		int pipefd[2];
		char **argv;
	};
	__attribute__((noreturn)) void exit_error(const char *str) {
		perror(str);
		_exit(1);
	}
	__attribute__((noreturn)) void exit_fail(const char *str) {
		fprintf(stderr, "%s\n", str);
		_exit(1);
	}
	void clear_all_caps() {
		cap_t caps = cap_get_proc();
		if (cap_clear(caps) == -1) exit_error("cap_clear");
		if (cap_set_proc(caps) == -1) exit_error("cap_set_proc");
		cap_free(caps);
	}
	int mkdir_p(const char *name) {
		if (const int pid = vfork()) {
			if (pid == -1) return -1;
			int st;
			waitpid(pid, &st, 0);
			return (WIFEXITED(st) && WEXITSTATUS(st) == 0) ? 0 : -1;
		} else {
			execl("/bin/mkdir", "/bin/mkdir", "-p", name, (void *)0);
			_exit(1);
		}
	}
	int proc(void *arg_) {
		prctl(PR_SET_PDEATHSIG, SIGKILL);
		close(static_cast<proc_arg_t *>(arg_)->pipefd[0]);
		const auto argv = static_cast<proc_arg_t *>(arg_)->argv;
		if (mount("/", "/", "none", MS_PRIVATE|MS_REC, nullptr) == -1) exit_error("mount --make-rprivate /");
		mkdir_p("jail/tmp");
		if (mount("jail", "jail", "none", MS_BIND, nullptr) == -1) exit_error("mount --bind /");
		for (const std::string d: static_cast<proc_arg_t *>(arg_)->mount_dirs) {
			const auto e = "jail" + d;
			mkdir_p(e.c_str());
			if (mount(d.c_str(), e.c_str(), "none", MS_BIND, nullptr) == -1) exit_error(("mount --bind " + d).c_str());
			if (mount(nullptr, e.c_str(), nullptr, MS_REMOUNT|MS_RDONLY|MS_BIND|MS_NOSUID, nullptr) == -1) exit_error(("mount -o remount,ro,bind,nosuid " + d).c_str());
		}
		mkdir_p("jail/home/jail");
		if (static_cast<proc_arg_t *>(arg_)->fakehome.empty()) {
			if (mount("jail/home/jail", "jail/home/jail", "none", MS_BIND, nullptr) == -1) exit_error("mount --bind /home/jail");
		} else {
			if (mount(static_cast<proc_arg_t *>(arg_)->fakehome.c_str(), "jail/home/jail", "none", MS_BIND, nullptr) == -1) exit_error("mount --bind /home/jail");
		}
		if (mount(nullptr, "jail/home/jail", nullptr, MS_REMOUNT|MS_BIND|MS_NOSUID, nullptr) == -1) exit_error("mount -o remount,bind,nosuid /home/jail");
		setenv("HOME", "/home/jail", 1);
		mkdir_p("jail/proc");
		mkdir_p("jail/dev");
		for (const std::string d: static_cast<proc_arg_t *>(arg_)->devices) {
			struct stat s;
			const std::string e = "jail" + d;
			unlink(e.c_str());
			if (stat(d.c_str(), &s) == -1) exit_error(("stat " + d).c_str());
			if (S_ISCHR(s.st_mode) || S_ISBLK(s.st_mode)) if (mknod(e.c_str(), s.st_mode, s.st_rdev) == -1) exit_error(("mknod " + d).c_str());
		}
		if (mount("jail/tmp", "jail/tmp", "none", MS_BIND, nullptr) == -1) exit_error("mount --bind /tmp");
		if (mount(nullptr, "jail", nullptr, MS_REMOUNT|MS_RDONLY|MS_BIND|MS_NOSUID, nullptr) == -1) exit_error("mount -o remount,ro,bind,nosuid /");
		if (chroot("jail") == -1) exit_error("chroot");
		chdir("/home/jail");
		if (mount("proc", "/proc", "proc", MS_RDONLY|MS_NOSUID|MS_NOEXEC|MS_NODEV, nullptr) == -1) exit_error("mount -o ro,nosuid,noexec,nodev /proc");
		{
			sigset_t sigs;
			sigfillset(&sigs);
			sigprocmask(SIG_BLOCK, &sigs, nullptr);
		}
		if (const int pid = fork()) {
			if (pid == -1) exit_error("fork");
			const int st = wait_and_forward_signals(pid);
			write(static_cast<proc_arg_t *>(arg_)->pipefd[1], &st, sizeof(st));
		} else {
			prctl(
				PR_SET_SECUREBITS,
				SECBIT_KEEP_CAPS | SECBIT_KEEP_CAPS_LOCKED |
					SECBIT_NO_SETUID_FIXUP | SECBIT_NO_SETUID_FIXUP_LOCKED |
					SECBIT_NOROOT | SECBIT_NOROOT_LOCKED);
			clear_all_caps();
			{
				sigset_t sigs;
				sigfillset(&sigs);
				sigprocmask(SIG_UNBLOCK, &sigs, nullptr);
			}
			if (argv[0]) execv(argv[0], argv);
			else execl("/bin/sh", "/bin/sh", (void *)0);
			exit_error("execve");
		}
		return 0;
	}
	void print_help() { }
	int exit_help(const char *) {
		return 1;
	}
	static const int stacksize = 4096;
	int main(int argc, char **argv) {
		char stack[stacksize];
		proc_arg_t args = { };

		{
			static const option opts[] = {
				{ "mounts", 1, nullptr, 'm' },
				{ "devices", 1, nullptr, 'd' },
				{ "fakehome", 1, nullptr, 'h' },
				{ nullptr, 0, nullptr, 0 },
			};
			for (int opt; (opt = getopt_long(argc, argv, "m:d:u:g:h:", opts, nullptr)) != -1; )
			switch (opt) {
			case 'm':
				{
					std::string s(optarg);
					boost::algorithm::split(args.mount_dirs, s, boost::algorithm::is_any_of(","));
				}
				break;
			case 'd':
				{
					std::string s(optarg);
					boost::algorithm::split(args.devices, s, boost::algorithm::is_any_of(","));
				}
				break;
			case 'h':
				args.fakehome = optarg;
				break;
			default:
				print_help();
				return 1;
			}
		}
		pipe2(args.pipefd, O_CLOEXEC);
		args.argv = argv + optind;

		{
			cap_t caps = cap_get_proc();
			cap_value_t cap_list[] = { CAP_SYS_ADMIN, CAP_SYS_CHROOT, CAP_MKNOD };
			if (cap_set_flag(caps, CAP_EFFECTIVE, sizeof(cap_list)/sizeof(cap_list[0]), cap_list, CAP_SET) == -1) exit_error("cap_set_flag");
			if (cap_set_proc(caps) == -1) exit_error("cap_set_proc");
			cap_free(caps);
		}

		int pid = ::clone(
			&proc,
			stack + stacksize,
			SIGCHLD | CLONE_NEWIPC | CLONE_NEWNET | CLONE_NEWNS | CLONE_NEWPID | CLONE_NEWUTS,
			&args);
		if (pid == -1) {
			perror("clone");
			return 1;
		}
		clear_all_caps();
		close(args.pipefd[1]);

		int st = wait_and_forward_signals(pid);
		int buf;
		if (read(args.pipefd[0], &buf, sizeof(buf)) == 4) st = buf;
		close(args.pipefd[0]);
		{
			sigset_t sigs;
			sigfillset(&sigs);
			sigprocmask(SIG_UNBLOCK, &sigs, nullptr);
		}
		if (WIFEXITED(st)) return WEXITSTATUS(st);
		if (WIFSIGNALED(st)) raise(WTERMSIG(st));
		return 1;
	}
}
}

int main(int argc, char **argv) {
	return wandbox::jail::main(argc, argv);
}
