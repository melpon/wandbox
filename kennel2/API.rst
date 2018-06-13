API
============

API home is ``https://wandbox.org/api``

GET /list.json
--------------

List compiler informations.

Parameter
^^^^^^^^^

Nothing.

Sample
^^^^^^

::

  $ curl https://wandbox.org/api/list.json
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
    "templates":["gcc"]
  },{
    ...
  }]

POST /compile.json
------------------

Compile posted code.

This API accepts only "application/json" in a "Content-Type" header.

Parameter
^^^^^^^^^

compiler [String]
  Used compiler name.
code [String]
  Compiled code.
codes [Array of Object{"file":[String], "code":[String]}] (optional, default is an empty array)
  Additional codes.
options [String] (optional, default is an empty string)
  Used options for a compiler joined by comma.
stdin [String] (optional, default is an empty string)
  Stdin
compiler-option-raw [String] (optional, default is an empty string)
  Compile-time any additional options joined by line-break.
runtime-option-raw [String] (optional, default is an empty string)
  Run-time any additional options joined by line-break.
save [Bool] (optional, default is false)
  Generate permanent link if true.

Result
^^^^^^

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
permlink (only ``save`` is true)
  ``permlink`` is you can pass to `GET /permlink/:link`_.
url (only ``save`` is true)
  URL to display on browser.

Sample
^^^^^^

::

  $ cat test.json
  {
    "code":"#include <iostream>\nint main() { int x = 0; std::cout << \"hoge\" << std::endl; }",
    "options": "warning,gnu++1y",
    "compiler": "gcc-head",
    "compiler-option-raw": "-Dx=hogefuga\n-O3"
  }
  $ curl -H "Content-type: application/json" -d @test.json  https://wandbox.org/api/compile.json
  {
    "status":"0",
    "compiler_message":"prog.cc: In function 'int main()':\n\u003ccommand-line\u003e:0:3: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
    "program_message":"hoge\n",
    "compiler_error":"prog.cc: In function 'int main()':\n\u003ccommand-line\u003e:0:3: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout \u003c\u003c \"hoge\" \u003c\u003c std::endl; }\n                  ^\n",
    "program_output":"hoge\n"
  }

POST /compile.ndjson
--------------------

Compile posted code and return multiple JSONs as `NDJSON <http://specs.okfnlabs.org/ndjson/>`_

This API accepts only "application/json" in a "Content-Type" header.

Parameter
^^^^^^^^^

Same as `POST /compile.json`_ Parameter. But `save` option is not implemented now.

Result
^^^^^^

Return multiple JSONs as NDJSON.

``Content-Type`` in the response header is ``application/x-ndjson``.

``{"type": "Control": "data": "Start"}``
  Start compilation and running.
``{"type": "Control": "data": "Finish"}``
  Finish compilation and running.
``{"type": "CompilerMessageS": "data": <string>}``
  Stdout output by the compiler.
``{"type": "CompilerMessageE": "data": <string>}``
  Stderr output by the compiler.
``{"type": "StdOut", "data": <string>}``
  Stdout output by the program.
``{"type": "StdErr", "data": <string>}``
  Stderr output by the program.
``{"type": "ExitCode", "data": <string>}``
  Exit code returned by the program.
``{"type": "Signal", "data": <string>}``
  Signal code.

Sample
^^^^^^

::

  $ cat test.json
  {
    "code":"#include <iostream>\nint main() { int x = 0; std::cout << \"hoge\" << std::endl; }",
    "options": "warning,gnu++1y",
    "compiler": "gcc-head",
    "compiler-option-raw": "-Dx=hogefuga\n-O3"
  }
  $ curl -H "Content-type: application/json" -d @test.json  https://wandbox.org/api/compile.json
  {"data":"Start","type":"Control"}
  {"data":"prog.cc: In function 'int main()':\n<command-line>: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout << \"hoge\" << std::endl; }\n                  ^\n","type":"CompilerMessageE"}
  {"data":"hoge\n","type":"StdOut"}
  {"data":"0","type":"ExitCode"}
  {"data":"Finish","type":"Control"}

GET /permlink/:link
-------------------

Get a result specified a permanent link ``:link``

Parameter
^^^^^^^^^

Nothing.

Result
^^^^^^

parameter
  Same as `POST /compile.json`_ Parameter with ``created-at`` is a compiled time formatted by ISO 8601, and without ``save``.
result
  Same as `POST /compile.json`_ Result without ``permlink`` and ``url``.

GET /template/:template-name
--------------

Get a template code for ``:template-name``.

Parameter
^^^^^^^^^

Nothing.

Sample
^^^^^^

::

  $ curl https://wandbox.org/api/template/gcc
  {
    "code": "// This file is a \"Hello, world!\" in C++ language by gcc for wandbox.\n#include <iostream>\n#include <cstdlib>\n\nint main()\n{\n    std::cout << \"Hello, Wandbox!\" << std::endl;\n}\n\n// C++ language references:\n//   https://msdn.microsoft.com/library/3bstk3k5.aspx\n//   http://www.cplusplus.com/\n//   https://isocpp.org/\n//   http://www.open-std.org/jtc1/sc22/wg21/\n\n// Boost libraries references:\n//   http://www.boost.org/doc/\n"
  }

GET /user.json
--------------

Check the user is logged in.

Parameter
^^^^^^^^^

session
  Session key passed by Wandbox.

Result
^^^^^^

login
  ``true`` If the user is logged in.
username
  The user's GitHub login name If ``login`` parameter is ``true``, otherwise the parameter is not set.

Sample
^^^^^^

::

  $ curl https://wandbox.org/api/user.json?session=zi35OwVNg0SwKMQo3VpfZeWxuXSyQ2nA
  {"login":true,"username":"melpon"}

Plugin
------------

VIM Plugin for Wandbox is here_. thanks @rhysd for your contribution!

.. _here: https://github.com/rhysd/wandbox-vim
