#include <dirent.h>
#include <fcntl.h>
#include <getopt.h>
#include <grp.h>
#include <libgen.h>
#include <linux/securebits.h>
#include <pwd.h>
#include <sched.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/capability.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include <boost/asio.hpp>
#include <boost/format.hpp>
#include <boost/fusion/adapted/std_pair.hpp>
#include <boost/optional.hpp>
#include <boost/spirit/include/qi.hpp>
#include <functional>
#include <iterator>
#include <random>
#include <string>
#include <vector>

namespace wandbox {
namespace jail {
int wait_and_forward_signals(int primary_child_pid, bool wait_grandchilds) {
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
      const int r = waitpid(-1, &st_, WNOHANG | __WALL);
      if (r == -1) return;
      if (r == primary_child_pid) {
        if (WIFEXITED(st_) || WIFSIGNALED(st_)) {
          ret = st_;
          if (!wait_grandchilds) return;
          waited = true;
        } else {
          break;
        }
      }
      if (sig == -1) break;
      if (r == 0) {
        if (!waited) kill(primary_child_pid, sig);
        break;
      }
    }
    sigs.async_wait(std::ref(f));
  };
  {
    sigset_t sigs;
    sigfillset(&sigs);
    sigprocmask(SIG_UNBLOCK, &sigs, nullptr);
  }
  aio.post(std::bind(std::ref(f), boost::system::error_code{}, -1));
  aio.run();
  for (int n = 0; n < 256; ++n) sigaction(n, &actions[n], nullptr);
  return ret;
}
struct mount_target {
  std::string realdir;
  std::string mountpoint;
  bool writable;
};
struct device_file {
  std::string filename;
  mode_t mode;
  dev_t dev;
};
struct proc_arg_t {
  std::string rootdir;
  std::string startdir;
  std::vector<mount_target> mounts;
  std::vector<device_file> devices;
  bool kill_grandchilds;
  int pipefd[2];
  unsigned newuid;
  char** argv;
};
__attribute__((noreturn)) void exit_error(const char* str) {
  perror(str);
  _exit(1);
}
__attribute__((noreturn)) void exit_fail(const char* str) {
  fprintf(stderr, "%s\n", str);
  _exit(1);
}
void clear_all_caps() {
  cap_t caps = cap_get_proc();
  if (cap_clear(caps) == -1) exit_error("cap_clear");
  if (cap_set_proc(caps) == -1) exit_error("cap_set_proc");
  cap_free(caps);
}
int mkdir_p(const char* name) {
  if (const int pid = vfork()) {
    if (pid == -1) return -1;
    int st;
    waitpid(pid, &st, 0);
    return (WIFEXITED(st) && WEXITSTATUS(st) == 0) ? 0 : -1;
  } else {
    execl("/bin/mkdir", "/bin/mkdir", "-p", name, (void*)0);
    _exit(1);
  }
}
int rm_rf(const char* name) {
  if (const int pid = vfork()) {
    if (pid == -1) return -1;
    int st;
    waitpid(pid, &st, 0);
    return (WIFEXITED(st) && WEXITSTATUS(st) == 0) ? 0 : -1;
  } else {
    execl("/bin/rm", "/bin/rm", "-rf", name, (void*)0);
    _exit(1);
  }
}
int chown_r(const char* name, unsigned uid, unsigned gid) {
  static bool (*const f)(unsigned, unsigned, DIR*) = [](unsigned uid,
                                                        unsigned gid, DIR* d) {
    if (!d) return true;
    bool failed = false;
    while (auto p = readdir(d)) {
      if (strcmp(p->d_name, ".") == 0 || strcmp(p->d_name, "..") == 0) continue;
      failed |=
          fchownat(dirfd(d), p->d_name, uid, gid, AT_SYMLINK_NOFOLLOW) == -1;
      int fd = openat(dirfd(d), p->d_name, O_RDONLY | O_DIRECTORY | O_NOFOLLOW);
      if (fd == -1) continue;
      failed |= !f(uid, gid, fdopendir(fd));
    }
    closedir(d);
    return !failed;
  };
  chown(name, uid, gid);
  return f(uid, gid, opendir(name)) ? 0 : -1;
}
std::string catpath(const std::string& dir, const std::string& file) {
  if (file.empty()) return dir;
  if (dir.empty()) return file;
  if (file.front() == '/' && dir.back() == '/') return dir + file.substr(1);
  if (file.front() == '/' || dir.back() == '/') return dir + file;
  return dir + "/" + file;
}
int proc(void* arg_) {
  prctl(PR_SET_PDEATHSIG, SIGKILL);
  const auto& arg = *static_cast<proc_arg_t*>(arg_);
  close(arg.pipefd[0]);
  const auto& argv = arg.argv;

  // activate loopback interface
  {
    ifreq ifr;
    strncpy(ifr.ifr_name, "lo", IFNAMSIZ);
    const int fd = socket(PF_INET, SOCK_DGRAM, 0);
    if (fd == -1) exit_error("socket(PF_INET, SOCK_DGRAM, 0)");
    if (ioctl(fd, SIOCGIFFLAGS, &ifr)) close(fd), exit_error("SIOCGIFFLAGS");
    ifr.ifr_flags |= IFF_UP;
    if (ioctl(fd, SIOCSIFFLAGS, &ifr)) close(fd), exit_error("SIOCSIFFLAGS");
  }

  // adjust uid/gid
  if (chown(".", arg.newuid, arg.newuid) == -1) exit_error("chown .");
  if (setuid(arg.newuid) == -1 || setgid(arg.newuid) == -1)
    exit_error("setuid");
  if (chmod(".", 0755) == -1) exit_error("chmod .");
  if (chown_r(".", arg.newuid, arg.newuid) == -1) exit_error("chown -r .");

  // prepare root directory
  const auto& rootdir = arg.rootdir;
  if (mount("/", "/", "none", MS_PRIVATE | MS_REC, nullptr) == -1)
    exit_error("mount --make-rprivate /");
  mkdir_p(rootdir.c_str());
  if (mount("none", rootdir.c_str(), "tmpfs", 0, "") == -1)
    exit_error(("mount -t tmpfs " + rootdir).c_str());

  // mount binds
  for (const auto& m : arg.mounts) {
    const auto& d = m.realdir;
    const auto e = catpath(rootdir, m.mountpoint);
    mkdir_p(e.c_str());
    if (mount(d.c_str(), e.c_str(), "none", MS_BIND, nullptr) == -1)
      exit_error(("mount --bind " + d + " " + e).c_str());
    if (mount(nullptr, e.c_str(), nullptr,
              MS_REMOUNT | (m.writable ? 0 : MS_RDONLY) | MS_BIND | MS_NOSUID,
              nullptr) == -1)
      exit_error(("mount -o remount,bind,nosuid " + e).c_str());
  }

  // create device files
  for (const auto& d : arg.devices) {
    const auto& f = catpath(rootdir, d.filename);
    std::vector<char> x(f.begin(), f.end());
    mkdir_p(dirname(&x[0]));
    if (mknod(f.c_str(), d.mode, d.dev) == -1)
      exit_error(("mknod " + f).c_str());
  }

  // mount /proc
  mkdir_p(catpath(rootdir, "proc").c_str());
  if (mount("proc", catpath(rootdir, "proc").c_str(), "proc",
            MS_RDONLY | MS_NOSUID | MS_NOEXEC | MS_NODEV, nullptr) == -1)
    exit_error("mount -o ro,nosuid,noexec,nodev /proc");

  // finish
  if (mount(nullptr, rootdir.c_str(), nullptr,
            MS_REMOUNT | MS_RDONLY | MS_BIND | MS_NOSUID, nullptr) == -1)
    exit_error("mount -o remount,ro,bind,nosuid /");
  if (chroot(rootdir.c_str()) == -1) exit_error(("chroot " + rootdir).c_str());
  if (chdir(arg.startdir.c_str()) == -1)
    exit_error(("chdir " + arg.startdir).c_str());
  {
    sigset_t sigs;
    sigfillset(&sigs);
    sigprocmask(SIG_BLOCK, &sigs, nullptr);
  }
  if (const int pid = fork()) {
    if (pid == -1) exit_error("fork");
    const int st = wait_and_forward_signals(pid, !arg.kill_grandchilds);
    const int fd = arg.pipefd[1];
    if (write(fd, &st, sizeof(st)) == -1) exit_error("write");
    close(fd);
  } else {
    prctl(PR_SET_SECUREBITS, SECBIT_KEEP_CAPS | SECBIT_KEEP_CAPS_LOCKED |
                                 SECBIT_NO_SETUID_FIXUP |
                                 SECBIT_NO_SETUID_FIXUP_LOCKED | SECBIT_NOROOT |
                                 SECBIT_NOROOT_LOCKED);
    clear_all_caps();
    {
      sigset_t sigs;
      sigfillset(&sigs);
      sigprocmask(SIG_UNBLOCK, &sigs, nullptr);
    }
    setsid();
    if (argv[0])
      execv(argv[0], argv);
    else
      execl("/bin/sh", "/bin/sh", (void*)0);
    exit_error("execve");
  }
  return 0;
}
void print_help() {}
int exit_help(const char*) { return 1; }

static const int stacksize = 4096;
int main(int argc, char** argv) {
  prctl(PR_SET_PDEATHSIG, SIGKILL);
  if (kill(getppid(), 0) < 0) raise(SIGKILL);

  char stack[stacksize];
  proc_arg_t args = {".", "/", {}, {}, false, {-1, -1}, getuid(), nullptr};

  {
    static const option opts[] = {
        {"mounts", 1, nullptr, 'm'},  {"rwmounts", 1, nullptr, 'w'},
        {"devices", 1, nullptr, 'd'}, {"rootdir", 1, nullptr, 'r'},
        {"chdir", 1, nullptr, 'c'},   {"kill", 0, nullptr, 'k'},
        {"uids", 1, nullptr, 'u'},    {nullptr, 0, nullptr, 0},
    };
    for (int opt;
         (opt = getopt_long(argc, argv, "m:d:u:g:h:", opts, nullptr)) != -1;)
      switch (opt) {
        case 'm':
        case 'w': {
          namespace qi = boost::spirit::qi;
          const auto* ite = optarg;
          const auto end = ite + strlen(optarg);
          std::vector<std::pair<std::string, boost::optional<std::string> > >
              mounts;
          qi::parse(ite, end,
                    (qi::as_string[+(qi::char_ - ',' - '=')] >
                     -qi::as_string[('=' > +(qi::char_ - ','))]) %
                        ',',
                    mounts);
          for (auto& m : mounts) {
            if (m.first.empty()) continue;
            args.mounts.push_back(
                {m.second ? *m.second : m.first, m.first, opt == 'w'});
          }
        } break;
        case 'd': {
          namespace qi = boost::spirit::qi;
          const auto* ite = optarg;
          const auto end = ite + strlen(optarg);
          std::vector<std::pair<
              std::string, boost::optional<std::pair<unsigned, unsigned> > > >
              devices;
          qi::parse(ite, end,
                    (qi::as_string[+(qi::char_ - ',' - '=')] >
                     -('=' > qi::uint_ > ',' > qi::uint_)) %
                        ',',
                    devices);
          for (auto& d : devices) {
            if (d.first.empty()) continue;
            device_file x = {
                std::move(d.first), d.second ? (S_IFCHR | 0666) : 0u,
                d.second ? makedev(d.second->first, d.second->second) : 0};
            if (!d.second) {
              struct stat s;
              if (stat(x.filename.c_str(), &s) == -1)
                exit_error(("cannot stat " + x.filename).c_str());
              if (!S_ISCHR(s.st_mode))
                exit_error((x.filename + " is not a character device").c_str());
              x.mode = s.st_mode;
              x.dev = s.st_rdev;
            }
            args.devices.emplace_back(std::move(x));
          }
        } break;
        case 'u': {
          namespace qi = boost::spirit::qi;
          const auto* ite = optarg;
          const auto end = ite + strlen(optarg);
          std::pair<unsigned, unsigned> uids;
          qi::parse(ite, end, (qi::uint_ > ':' > qi::uint_), uids);
          std::minstd_rand g((getpid() << 16) ^ std::time(nullptr));
          args.newuid = std::uniform_int_distribution<unsigned>(uids.first,
                                                                uids.second)(g);
        } break;
        case 'r':
          args.rootdir = optarg;
          break;
        case 'c':
          args.startdir = optarg;
          break;
        case 'k':
          args.kill_grandchilds = true;
          break;
        case 'h':
        default:
          print_help();
          return 1;
      }
  }
  if (pipe2(args.pipefd, O_CLOEXEC) == -1) exit_error("pipe");
  args.argv = argv + optind;

  {
    cap_t caps = cap_get_proc();
    cap_value_t cap_list[] = {CAP_SYS_ADMIN, CAP_SYS_CHROOT, CAP_MKNOD,
                              CAP_SETUID,    CAP_SETGID,     CAP_CHOWN,
                              CAP_NET_ADMIN};
    if (cap_set_flag(caps, CAP_EFFECTIVE,
                     sizeof(cap_list) / sizeof(cap_list[0]), cap_list,
                     CAP_SET) == -1)
      exit_error("cap_set_flag");
    if (cap_set_proc(caps) == -1) exit_error("cap_set_proc");
    cap_free(caps);
  }

  int pid = ::clone(&proc, stack + stacksize,
                    SIGCHLD | CLONE_NEWIPC | CLONE_NEWNET | CLONE_NEWNS |
                        CLONE_NEWPID | CLONE_NEWUTS,
                    &args);
  if (pid == -1) {
    perror("clone");
    return 1;
  }
  clear_all_caps();
  close(args.pipefd[1]);

  int st = wait_and_forward_signals(pid, !args.kill_grandchilds);
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
}  // namespace jail
}  // namespace wandbox

int main(int argc, char** argv) { return wandbox::jail::main(argc, argv); }
