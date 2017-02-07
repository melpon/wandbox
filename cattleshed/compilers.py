import json

def merge(*args):
    result = {}
    for dic in args:
        result.update(dic)
    return result

class Switches(object):
    def resolve_conflicts(self, pairs):
        conflicts = [p[0] for p in pairs]
        return [(p[0], merge(p[1], { "conflicts": conflicts })) for p in pairs]

    def make_boost_header(self):
        pairs = [
            ("boost-nothing-header", {
                "flags":[],
                "display-name":"Don't Use Boost",
                "display-flags":"",
                "runtime":True,
            }),
            ("boost-1.55-header", {
                "flags":["-I/usr/local/boost-1.55.0/include"],
                "display-name":"Boost 1.55.0",
                "display-flags":"-I/usr/local/boost-1.55.0/include",
                "runtime":True,
            }),
        ]
        return self.resolve_conflicts(pairs)

    def make_default(self):
        return {
            "mono-optimize":{
                "flags":"-optimize",
                "display-name":"Optimization",
            },
            "cpp-p":{
                "flags":["-P"],
                "display-name":"-P",
                "runtime":True,
            },
            "perl5.18.0":{
                "flags":"-M5.18.0",
                "display-name":"-M5.18.0",
                "runtime":True,
                "insert-position":1,
            },
            "delphi-mode":{
                "flags":["-Mdelphi"],
                "display-name":"Delphi 7 mode",
            },
            "node-harmony":{
                "flags":["--harmony"],
                "display-name":"--harmony",
                "insert-position":1,
                "runtime":True,
            },
            "coffee-compile-only":{
                "flags":["-p"],
                "display-name":"Compile Only",
                "insert-position":2,
                "runtime":True,
            },
        }

    def make(self):
        return merge(
            self.make_default(),
            self.make_boost_header())

class Compilers(object):
    def resolve_format(self, formats, **kwargs):
        def typedo(value):
            if isinstance(value, str):
                return value.format(**kwargs)
            elif isinstance(value, (list, dict)):
                return self.resolve_format(value, **kwargs)
            else:
                return value

        if isinstance(formats, list):
            return [typedo(value) for value in formats]
        else:
            return {key:typedo(value) for key,value in formats.iteritems()}

    def make_common(self, names, formats):
        params = {
            name[0]: self.resolve_format(formats, name=name[0], **name[1]["params"])
            for name in names
        }
        after = {
            name[0]: name[1]["after"]
            for name in names
        }
        compilers = [merge(
            { "name": name[0] },
            params[name[0]],
            after[name[0]],
        ) for name in names]

        return compilers

    def make_mono(self):
        NAMES = [
            ("mcs-2.6.7", {
                "params": {
                    "mono": "mono-2.6.7",
                },
                "after": {
                    "display-name":"gmcs for Unity on iOS",
                    "display-compile-command":"gmcs -out:prog.exe prog.cs",
                    "compile-command":["/usr/local/mono-2.6.7/bin/gmcs-custom", "-out:prog.exe", "prog.cs"],
                    "run-command":["/usr/local/mono-2.6.7/bin/mono-custom", "prog.exe"]
                },
            }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"C#",
            "output-file":"prog.cs",
            "compiler-option-raw":True,
            "compile-command":["/usr/local/{mono}/bin/mcs", "-out:prog.exe", "prog.cs"],
            "version-command":["/bin/sh", "-c", "/usr/local/{mono}/bin/mcs --version | head -1 | cut -d' ' -f5"],
            "swithes":["mono-optimize"],
            "initial-checked":[],
            "display-name":"mcs",
            "display-compile-command":"mcs -out:prog.exe prog.cs",
            "run-command":["/usr/local/{mono}/bin/mono", "prog.exe"]
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_perl(self):
        NAMES = [
            ("perl-head", {
                "params": {
                    "bin": "perl",
                },
                "after": {
                    "display-name":"perl-devel HEAD",
                    "display-compile-command":"perl prog.pl",
                },
            }),
            ("perl-5.19.2", {
                "params": {
                    "bin": "perl5.19.2",
                },
                "after": {
                    "display-name":"perl-devel",
                    "display-compile-command":"perl5.19.2 prog.pl",
                },
            }),
            ("perl-5.18.0", {
                "params": {
                    "bin": "perl",
                },
                "after": {
                    "display-name":"perl",
                    "display-compile-command":"perl prog.pl",
                    "switches":["perl5.18.0"],
                    "initial-checked":["perl5.18.0"],
                },
            }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.pl",
            "run-command":["/usr/local/{name}/bin/{bin}", "prog.pl"],
            "language":"Perl",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/usr/local/{name}/bin/{bin}", "-e", "print $^V"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_python(self):
        NAMES = [
           ("python-head", {
                "params": {
                    "bin": "python3",
                },
                "after": {
                    "display-name":"python HEAD",
                },
           }),
           ("python-2.7-head", {
                "params": {
                    "bin": "python",
                },
                "after": {
                    "display-name":"python2.7 HEAD",
                },
           }),
           ("python-3.5.1", {
                "params": {
                    "bin": "python3",
                },
                "after": {
                },
           }),
           ("python-3.5.0", {
                "params": {
                    "bin": "python3",
                },
                "after": {
                },
           }),
           ("python-3.4.3", {
                "params": {
                    "bin": "python3",
                },
                "after": {
                },
           }),
           ("python-3.3.2", {
                "params": {
                    "bin": "python3",
                },
                "after": {
                },
           }),
           ("python-2.7.3", {
                "params": {
                    "bin": "python",
                },
                "after": {
                },
           }),
           ("pypy-2.1", {
                "params": {
                    "bin": "pypy",
                },
                "after": {
                    "display-name":"pypy",
                    "display-compile-command":"pypy prog.py",
                    "version-command":["/usr/local/pypy-2.1/bin/pypy", "-c", "import sys; print(sys.version.split()[7])"],
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.py",
            "run-command":["/usr/local/{name}/bin/{bin}", "prog.py"],
            "display-name":"python",
            "display-compile-command":"python prog.py",
            "language":"Python",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/usr/local/{name}/bin/{bin}", "-c", "import sys; print(sys.version.split()[0])"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_ruby(self):
        NAMES = [
           ("ruby-head", {
                "params": {
                    "bin": "ruby",
                },
                "after": {
                    "display-name": "ruby HEAD",
                },
           }),
           ("ruby-2.0.0-p247", {
                "params": {
                    "bin": "ruby",
                },
                "after": {
                },
           }),
           ("ruby-1.9.3-p0", {
                "params": {
                    "bin": "ruby",
                },
                "after": {
                },
           }),
           ("mruby-head", {
                "params": {
                    "bin": "mruby",
                },
                "after": {
                    "display-name": "mruby HEAD",
                    "display-compile-command":"mruby prog.rb",
                    "version-command":["/bin/sh", "-c", "cd /usr/local/mruby-head && git rev-parse HEAD | cut -c 1-8"],
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.rb",
            "run-command":["/usr/local/{name}/bin/{bin}", "prog.rb"],
            "display-name":"ruby",
            "display-compile-command":"ruby prog.rb",
            "language":"Ruby",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/usr/local/{name}/bin/ruby", "-e", "print RUBY_VERSION"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_php(self):
        NAMES = [
           ("php-head", {
                "params": {
                },
                "after": {
                    "display-name": "php HEAD",
                },
           }),
           ("php-5.5.6", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.php",
            "display-name":"php",
            "display-compile-command":"php prog.php",
            "language":"PHP",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "run-command":["/usr/local/{name}/bin/php", "prog.php"],
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/php -v | head -1 | cut -d' ' -f2"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_node(self):
        NAMES = [
           ("node-head", {
                "params": {
                },
                "after": {
                    "display-name": "node HEAD",
                    "switches":["node-harmony"],
                    "initial-checked":["node-harmony"],
                },
           }),
           ("node-0.10.24", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"JavaScript",
            "output-file":"prog.js",
            "display-name":"node",
            "display-compile-command":"node prog.js",
            "compile-command":"/bin/true",
            "run-command":["/usr/local/{name}/bin/node", "prog.js"],
            "runtime-option-raw":True,
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/node --version | cut -c2-"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_spidermonkey(self):
        return [{
            "name":"mozjs-24.2.0",
            "displayable":True,
            "language":"JavaScript",
            "output-file":"prog.js",
            "display-name":"SpiderMonkey",
            "display-compile-command":"js24 prog.js",
            "compile-command":"/bin/true",
            "run-command":["/usr/local/mozjs-24.2.0/bin/js24", "prog.js"],
            "runtime-option-raw":True,
            "version-command":["/bin/sh", "-c", "/usr/local/mozjs-24.2.0/bin/js24 --help | grep Version | cut -d'-' -f2 | cut -c2-"],
        }]

    def make_coffee_script(self):
        NAMES = [
           ("coffee-script-head", {
                "params": {
                },
                "after": {
                    "display-name":"coffee HEAD",
                    "version-command":["/bin/sh", "-c", "cd /usr/local/coffee-script-head && git rev-parse HEAD | cut -c 1-10"],
                },
           }),
           ("coffee-script-1.7.1", {
                "params": {
                },
                "after": {
                },
           }),
           ("coffee-script-1.6.3", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"CoffeeScript",
            "output-file":"prog.coffee",
            "display-name":"coffee",
            "display-compile-command":"coffee prog.coffee",
            "compile-command":"/bin/true",
            "run-command":["/usr/local/node-0.10.24/bin/node", "/usr/local/{name}/bin/coffee", "prog.coffee"],
            "runtime-option-raw":True,
            "version-command":["/bin/sh", "-c", "/usr/local/node-0.10.24/bin/node /usr/local/{name}/bin/coffee --version | cut -d' ' -f3"],
            "switches":["coffee-compile-only"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_sqlite(self):
        NAMES = [
           ("sqlite-head", {
                "params": {
                },
                "after": {
                    "display-name":"sqlite HEAD",
                },
           }),
           ("sqlite-3.8.1", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.sql",
            "run-command":["/bin/sh", "-c", "cat prog.sql | /usr/local/{name}/bin/sqlite3"],
            "display-name":"sqlite",
            "display-compile-command":"cat prog.sql | sqlite3",
            "language":"SQL",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/sqlite3 -version | cut -d' ' -f1"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_scala(self):
        NAMES = [
           ("scala-2.12.x", {
                "params": {
                },
                "after": {
                    "display-name":"scala-2.12.x HEAD",
                    "compile-command":["/bin/sh", "-c", "JAVA_HOME=/usr/local/java8 /usr/local/scala-2.12.x/bin/scalac prog.scala"],
                    "version-command":["/bin/sh", "-c", "JAVA_HOME=/usr/local/java8 /usr/local/scala-2.12.x/bin/scala -version 2>&1 | cut -d' ' -f5"],
                },
           }),
           ("scala-2.11.x", {
                "params": {
                },
                "after": {
                    "display-name":"scala-2.11.x HEAD",
                },
           }),
           ("scala-2.11.2", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.scala",
            "run-command":["/bin/bash", "/usr/local/{name}/bin/run.sh"],
            "display-name":"scala",
            "display-compile-command":"scala prog.scala",
            "jail-name": "jvm",
            "language":"Scala",
            "runtime-option-raw":True,
            "compile-command":["/usr/local/{name}/bin/scalac", "prog.scala"],
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/scala -version 2>&1 | cut -d' ' -f5"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_lua(self):
        NAMES = [
           ("lua-5.3.0", {
                "params": {
                },
                "after": {
                },
           }),
           ("lua-5.2.2", {
                "params": {
                },
                "after": {
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.lua",
            "run-command":["/usr/local/{name}/bin/lua", "prog.lua"],
            "display-name":"lua",
            "display-compile-command":"lua prog.lua",
            "language":"Lua",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/lua -v | cut -d' ' -f2"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_vim(self):
        NAMES = [
           ("vim-7.4.1714", {
                "params": {
                },
                "after": {
                    "version-command":["/bin/echo", "vim 7.4.1714"],
                },
           }),
           ("vim-7.4.729", {
                "params": {
                },
                "after": {
                    "version-command":["/bin/echo", "vim 7.4.729"],
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.vim",
            "run-command":["/usr/local/{name}/bin/vim", "-X", "-N", "-u", "NONE", "-i", "NONE", "-V1", "-e", "--cmd", "source prog.vim | qall!"],
            "display-compile-command":"vim -X -N -u NONE -i NONE -V1 -e --cmd 'source proc.vim | qall!'",
            "language":"Vim script",
            "runtime-option-raw":True,
            "compile-command":["/bin/true"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_swift(self):
        NAMES = [
            ("swift-2.2", {
                "params": {
                },
                "after": {
                },
            }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"Swift",
            "output-file":"prog.swift",
            "compiler-option-raw":True,
            "compile-command":["/usr/local/{name}/bin/run-swift.sh", "/usr/local/{name}/bin/swiftc", "prog.swift"],
            "version-command":["/bin/sh", "-c", "/usr/local/{name}/bin/run-swift.sh /usr/local/{name}/bin/swiftc --version | head -1 | cut -d' ' -f3"],
            "display-name":"swift",
            "display-compile-command":"swiftc prog.swift",
            "run-command":["/usr/local/{name}/bin/run-swift.sh", "./prog"]
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_default1(self):
        COMPILERS = [{
            "name":"gcc-4.8.2-pp",
            "displayable":True,
            "display-name":"gcc",
            "display-compile-command":"gcc prog.cpp -E",
            "language":"CPP",
            "output-file":"prog.cpp",
            "compile-command":"/bin/true",
            "runtime-option-raw":True,
            "run-command":["/usr/local/gcc-4.8.2/bin/gcc", "-E", "prog.cpp"],
            "version-command":["/usr/local/gcc-4.8.2/bin/gcc", "-dumpversion"],
            "switches":["cpp-p", "boost-nothing-header", "boost-1.55-header"],
            "initial-checked":["cpp-p", "boost-1.55-header"],
        }]
        return COMPILERS

    def make_default2(self):
        COMPILERS = [{
            "name":"bash",
            "displayable":True,
            "output-file":"prog.sh",
            "run-command":["/bin/bash", "prog.sh"],
            "display-name":"bash",
            "display-compile-command":"bash prog.sh",
            "language":"Bash script",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/bin/sh", "-c", "/bin/bash --version | head -1"],
        },{
            "name":"lazyk",
            "displayable":True,
            "output-file":"prog.lazy",
            "display-name":"lazyk",
            "display-compile-command":"lazyk prog.lazy",
            "language":"Lazy K",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "run-command":["/usr/local/lazyk/bin/lazyk", "prog.lazy"],
            "version-command":["/bin/echo", "-e", "\\n"],
        },{
            "name":"clisp-2.49.0",
            "displayable":True,
            "output-file":"prog.lisp",
            "display-name":"CLISP",
            "display-compile-command":"clisp prog.lisp",
            "language":"Lisp",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "run-command":["/usr/local/clisp-2.49.0/bin/clisp", "prog.lisp"],
            "version-command":["/bin/sh", "-c", "/usr/local/clisp-2.49.0/bin/clisp --version | head -1 | cut -d' ' -f3"],
        },{
            "name":"fpc-2.6.2",
            "displayable":True,
            "output-file":"prog.pas",
            "display-name":"Free Pascal",
            "display-compile-command":"fpc prog.pas",
            "language":"Pascal",
            "runtime-option-raw":True,
            "compile-command":["/usr/local/fpc-2.6.2/bin/fpc", "-Fu/usr/local/fpc-2.6.2/lib/fpc/2.6.2/units/x86_64-linux/*", "prog.pas"],
            "run-command":["./prog"],
            "version-command":["/bin/sh", "-c", "/usr/local/fpc-2.6.2/bin/fpc | head -1 | cut -d' ' -f5"],
            "switches":["delphi-mode"],
        },{
            "name":"groovy-2.2.1",
            "displayable":True,
            "output-file":"prog.groovy",
            "display-name":"Groovy",
            "display-compile-command":"groovy prog.groovy",
            "jail-name":"jvm",
            "language":"Groovy",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "run-command":["/usr/local/groovy-2.2.1/bin/groovy", "prog.groovy"],
            "version-command":["/bin/sh", "-c", "/usr/local/groovy-2.2.1/bin/groovy --version | cut -d' ' -f3"],
        }]
        return COMPILERS

    def make(self):
        return (
            self.make_default1() +
            self.make_mono() +
            self.make_perl() +
            self.make_python() +
            self.make_ruby() +
            self.make_php() +
            self.make_node() +
            self.make_spidermonkey() +
            self.make_coffee_script() +
            self.make_sqlite() +
            self.make_scala() +
            self.make_lua() +
            self.make_vim() +
            self.make_swift() +
            self.make_default2()
        )

def make_config():
    return {
        "switches": Switches().make(),
        "compilers": Compilers().make(),
    }

if __name__ == '__main__':
    print json.dumps(make_config(), indent=4)
