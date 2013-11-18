API
========

API home is ``melpon.org/wandbox/api``

GET /list.json
  List compiler infos.

  Parameter is nothing.

  Sample::

    $ curl http://melpon.org/wandbox/api/list.json
    [{
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
  "application/x-www-form-urlencoded" in the Content-Type header.

  Parameter:

  compiler [String]
    Used compiler name.
  options [String]
    Used options for a compiler joined by comma.
  code [String]
    Compiled code.

  Sample::

    $ cat test.json
    {
      "code":"#include <iostream>\nint main() { int x = 0; std::cout << \"hoge\" << std::endl; }",
      "options": "warning,gnu++1y",
      "compiler": "gcc-head"
    }
    $ curl -H "Content-type: application/json" -d "`cat test.json`"  http://melpon.org/wandbox/api/compile.json
    {
      "status":"0",
      "compiler_message":"prog.cc: In function 'int main()':\nprog.cc:2:18: warning: unused variable 'x' [-Wunused-variable]\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
      "program_message":"hoge\n",
      "compiler_error":"prog.cc: In function 'int main()':\nprog.cc:2:18: warning: unused variable 'x' [-Wunused-variable]\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
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
      merged message compiler_output and compiler_error
    program_output
      stdout at runtime
    program_error
      stderr at runtime
    program_message
      merged message program_output and program_error
