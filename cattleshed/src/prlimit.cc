#include <stdlib.h>

#include <getopt.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <unistd.h>

namespace wandbox {
namespace prlimit {
int main(int argc, char** argv) {
  {
    static const option opts[] = {
        {"core", 1, nullptr, 'c'},       {"data", 1, nullptr, 'd'},
        {"nice", 1, nullptr, 'e'},       {"fsize", 1, nullptr, 'f'},
        {"sigpending", 1, nullptr, 'i'}, {"memlock", 1, nullptr, 'l'},
        {"rss", 1, nullptr, 'm'},        {"nofile", 1, nullptr, 'n'},
        {"msgqueue", 1, nullptr, 'q'},   {"stack", 1, nullptr, 's'},
        {"cpu", 1, nullptr, 't'},        {"nproc", 1, nullptr, 'u'},
        {"as", 1, nullptr, 'v'},         {"locks", 1, nullptr, 'x'},
        {"rttime", 1, nullptr, 'y'},     {nullptr, 0, nullptr, 0},
    };
    for (int opt;
         (opt = getopt_long(argc, argv, "c:d:e:f:i:l:m:n:q:s:t:u:v:x:y:", opts,
                            nullptr)) != -1;) {
      const unsigned val = atoi(optarg);
      const struct rlimit lim = {val, val};
      switch (opt) {
        case 'c':
          setrlimit(RLIMIT_CORE, &lim);
          break;
        case 'd':
          setrlimit(RLIMIT_DATA, &lim);
          break;
        case 'e':
          setrlimit(RLIMIT_NICE, &lim);
          break;
        case 'f':
          setrlimit(RLIMIT_FSIZE, &lim);
          break;
        case 'i':
          setrlimit(RLIMIT_SIGPENDING, &lim);
          break;
        case 'l':
          setrlimit(RLIMIT_MEMLOCK, &lim);
          break;
        case 'm':
          setrlimit(RLIMIT_RSS, &lim);
          break;
        case 'n':
          setrlimit(RLIMIT_NOFILE, &lim);
          break;
        case 'q':
          setrlimit(RLIMIT_MSGQUEUE, &lim);
          break;
        case 's':
          setrlimit(RLIMIT_STACK, &lim);
          break;
        case 't':
          setrlimit(RLIMIT_CPU, &lim);
          break;
        case 'u':
          setrlimit(RLIMIT_NPROC, &lim);
          break;
        case 'v':
          setrlimit(RLIMIT_AS, &lim);
          break;
        case 'x':
          setrlimit(RLIMIT_LOCKS, &lim);
          break;
        case 'y':
          setrlimit(RLIMIT_RTTIME, &lim);
          break;
        default:
          continue;
      }
    }
  }
  argv += optind;

  execv(argv[0], argv);
  return 1;
}
}  // namespace prlimit
}  // namespace wandbox

int main(int argc, char** argv) { return wandbox::prlimit::main(argc, argv); }
