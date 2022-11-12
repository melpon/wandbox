#!/bin/bash

ptracer() {
  src/ptracer.exe --config ./config -- "$@"
}
is-same-output() {
  test "$(ptracer "$@")" == "$("$@")"
}

set -e

# runnable a command
is-same-output /bin/echo hoge
# blockable accessing to disallowed paths
is-same-output /usr/bin/size /lib/libc.so.6
is-same-output /usr/bin/size /proc/self/exe
! ptracer /bin/ls / 2> /dev/null
! ptracer /usr/bin/size /bin/echo 2> /dev/null
# accessible to working directory
is-same-output /usr/bin/wc -c "$(ls -1 | head -1)"
# fail if trying to exec a command that does not exist
test ! -x /bin/this-command-should-not-exist && ! ptracer /bin/this-command-should-not-exist

# blockable execve(2)
test "$(ptracer test/exec.test)" == "blocked"
