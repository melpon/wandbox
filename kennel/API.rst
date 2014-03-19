API
============

API home is ``melpon.org/wandbox/api``

GET /list.json
  List compiler informations.

  Parameter is nothing.

  Sample::

    $ curl http://melpon.org/wandbox/api/list.json
    [{
      "compiler-option-raw":true,
      "runtime-option-raw":false,
      "display-compile-command":"g++ prog.cc",
      "switches":[{
        "default":true,
        "name":"warning",
        "display-flags":"-Wall -Wextra",
        "display-name":"Warnings"
      },{
        "default":false,
        "name":"optimize",
        "display-flags":"-O2 -march=native",
        "display-name":"Optimization"
      },{
        "default":false,
        "name":"cpp-verbose",
        "display-flags":"-v",
        "display-name":"Verbose"
      },{
        "default":"boost-1.55",
        "options":[{
          "name":"boost-nothing",
          "display-flags":"",
          "display-name":"Don't Use Boost"
        },{
          "name":"boost-1.47",
          "display-flags":"-I/usr/local/boost-1.47.0/include",
          "display-name":"Boost 1.47.0"
        },{
          "name":"boost-1.48",
          "display-flags":"-I/usr/local/boost-1.48.0/include",
          "display-name":"Boost 1.48.0"
        },{
          ...
        },{
          "name":"boost-1.55",
          "display-flags":"-I/usr/local/boost-1.55.0/include",
          "display-name":"Boost 1.55.0"
        }]
      },{
        "default":true,
        "name":"sprout",
        "display-flags":"-I/usr/local/sprout",
        "display-name":"Sprout"
      },{
        "default":"gnu++1y",
        "options":[{
          "name":"c++98",
          "display-flags":"-std=c++98 -pedantic",
          "display-name":"C++03"
        },{
          ...
        },{
          "name":"gnu++1y",
          "display-flags":"-std=gnu++1y",
          "display-name":"C++1y(GNU)"
        }]
      }],
      "name":"gcc-head",
      "version":"4.9.0 20131031 (experimental)",
      "language":"C++",
      "display-name":"gcc HEAD"
    },{
      ...
    }]

POST /compile.json
  Compile posted code.

  This API accepts "application/json" or
  "application/x-www-form-urlencoded" in a "Content-Type" header.

  Parameter:

  compiler [String]
    Used compiler name.
  code [String]
    Compiled code.
  options [String] (optional, default it a empty string)
    Used options for a compiler joined by comma.
  stdin [String] (optional, default is a empty string)
    Stdin
  compiler-option-raw [String] (optional, default is a empty string)
    Compile-time any additional options joined by line-break.
  runtime-option-raw [String] (optional, default is a empty string)
    Run-time any additional options joined by line-break.

  Sample::

    $ cat test.json
    {
      "code":"#include <iostream>\nint main() { int x = 0; std::cout << \"hoge\" << std::endl; }",
      "options": "warning,gnu++1y",
      "compiler": "gcc-head",
      "compiler-option-raw": "-Dx=hogefuga\n-O3"
    }
    $ curl -H "Content-type: application/json" -d "`cat test.json`"  http://melpon.org/wandbox/api/compile.json
    {
      "status":"0",
      "compiler_message":"prog.cc: In function 'int main()':\n\u003ccommand-line\u003e:0:3: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
      "program_message":"hoge\n",
      "compiler_error":"prog.cc: In function 'int main()':\n\u003ccommand-line\u003e:0:3: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
      "program_output":"hoge\n"
    }

  Result:

  status
    Exit code
  signal
    Signal message
  compiler_output
    stdout at compiling
  compiler_error
    stderr at compiling
  compiler_message
    merged messages compiler_output and compiler_error
  program_output
    stdout at runtime
  program_error
    stderr at runtime
  program_message
    merged messages program_output and program_error

GET /permlink/:link
  Get a result specified a permanent link ``link``

  Parameter is nothing.

  Result is same as ``POST /compile.json``

Plugin
------------

VIM Plugin for Wandbox is here_. thanks @rhysd for your contribution!

.. _here: https://github.com/rhysd/wandbox-vim
