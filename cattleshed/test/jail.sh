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
# blockable accessing to disallowed pathes
is-same-output /usr/bin/size /lib/libc.so.6
is-same-output /usr/bin/size /proc/self/exe
! ptracer /bin/ls / 2> /dev/null
! ptracer /usr/bin/size /bin/echo 2> /dev/null
# accessible to working directory
is-same-output /usr/bin/wc -c "$(ls -1 | head -1)"

