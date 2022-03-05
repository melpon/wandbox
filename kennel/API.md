# API

型の情報は [kennel.proto](/proto/kennel.proto) を参照すること。

## GET /api/list.json

コンパイラ一覧の情報を返す。

### レスポンス

`CompilerInfo` の配列。

### 例

```console
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
```

## POST /api/compile.json

コンパイルと実行を行う。

### リクエスト

`CompileParameter` 。

### レスポンス

`CompileResult` 。

### 例

```console
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
```

## POST /api/compile.ndjson

コンパイルと実行を行って [NDJSON](https://github.com/ndjson/ndjson-spec/blob/master/README.md) を返す。

### リクエスト

`CompileParameter` 。

### レスポンス

1行ごとに `CompileNdjsonResult` のデータがやってくる。

### 例

```console
$ cat test.json
{
  "code":"#include <iostream>\nint main() { int x = 0; std::cout << \"hoge\" << std::endl; }",
  "options": "warning,gnu++1y",
  "compiler": "gcc-head",
  "compiler-option-raw": "-Dx=hogefuga\n-O3"
}
$ curl -H "Content-type: application/json" -d @test.json  https://wandbox.org/api/compile.ndjson
{"data":"Start","type":"Control"}
{"data":"prog.cc: In function 'int main()':\n<command-line>: warning: unused variable 'hogefuga' [-Wunused-variable]\nprog.cc:2:18: note: in expansion of macro 'x'\n int main() { int x = 0; std::cout << \"hoge\" << std::endl; }\n                  ^\n","type":"CompilerMessageE"}
{"data":"hoge\n","type":"StdOut"}
{"data":"0","type":"ExitCode"}
{"data":"Finish","type":"Control"}
```

## POST /api/permlink

`/api/compile.ndjson` の結果を保存する。

### リクエスト

`PostPermlinkRequest` 。

### レスポンス

`PostPermlinkResponse` 。

## GET /api/permlink/:permlink

パーマネントリンク `:permlink` のデータを取得する

### レスポンス

`GetPermlinkResponse` 。

## GET /api/template/:template-name

`:template-name` のテンプレートコードを取得する。

### レスポンス

`Template` 。

### 例

```console
$ curl https://wandbox.org/api/template/gcc
{
  "code": "// This file is a \"Hello, world!\" in C++ language by gcc for wandbox.\n#include <iostream>\n#include <cstdlib>\n\nint main()\n{\n    std::cout << \"Hello, Wandbox!\" << std::endl;\n}\n\n// C++ language references:\n//   https://msdn.microsoft.com/library/3bstk3k5.aspx\n//   http://www.cplusplus.com/\n//   https://isocpp.org/\n//   http://www.open-std.org/jtc1/sc22/wg21/\n\n// Boost libraries references:\n//   http://www.boost.org/doc/\n"
}
```

## GET /api/sponsors.json

スポンサーの一覧を取得する。

### レスポンス

`SponsorResponse` 。