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

    def make_cxx(self):
        pairs = [
            ("c89", {
                "flags":["-std=c89", "-pedantic-errors"],
                "display-name":"C89",
            }),
            ("c99", {
                "flags":["-std=c99", "-pedantic-errors"],
                "display-name":"C99",
            }),
            ("c11", {
                "flags":["-std=c11", "-pedantic-errors"],
                "display-name":"C11",
            }),
            ("c++98", {
                "flags":["-std=c++98", "-pedantic"],
                "display-name":"C++03",
            }),
            ("gnu++98", {
                "flags":"-std=gnu++98",
                "display-name":"C++03(GNU)",
            }),
            ("c++0x", {
                "flags":["-std=c++0x", "-pedantic"],
                "display-name":"C++0x",
            }),
            ("gnu++0x", {
                "flags":"-std=gnu++0x",
                "display-name":"C++0x(GNU)",
            }),
            ("c++11", {
                "flags":["-std=c++11", "-pedantic"],
                "display-name":"C++11",
            }),
            ("gnu++11", {
                "flags":"-std=gnu++11",
                "display-name":"C++11(GNU)",
            }),
            ("c++1y", {
                "flags":["-std=c++1y", "-pedantic"],
                "display-name":"C++1y",
            }),
            ("gnu++1y", {
                "flags":"-std=gnu++1y",
                "display-name":"C++1y(GNU)",
            }),
            ("c++14", {
                "flags":["-std=c++14", "-pedantic"],
                "display-name":"C++14",
            }),
            ("gnu++14", {
                "flags":"-std=gnu++14",
                "display-name":"C++14(GNU)",
            }),
            ("c++1z", {
                "flags":["-std=c++1z", "-pedantic"],
                "display-name":"C++1z",
            }),
            ("gnu++1z", {
                "flags":"-std=gnu++1z",
                "display-name":"C++1z(GNU)",
            }),
        ]
        return self.resolve_conflicts(pairs)

    def make_boost(self):
        pairs = [
            ("boost-nothing", {
                "flags":[],
                "display-name":"Don't Use Boost",
                "display-flags":"",
            }),
            ("boost-1.47", {
                "flags":["-I/usr/local/boost-1.47.0/include", "-L/usr/local/boost-1.47.0/lib", "-Wl,-rpath,/usr/local/boost-1.47.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.47.0",
                "display-flags":"-I/usr/local/boost-1.47.0/include",
            }),
            ("boost-1.48", {
                "flags":["-I/usr/local/boost-1.48.0/include", "-L/usr/local/boost-1.48.0/lib", "-Wl,-rpath,/usr/local/boost-1.48.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.48.0",
                "display-flags":"-I/usr/local/boost-1.48.0/include",
            }),
            ("boost-1.49", {
                "flags":["-I/usr/local/boost-1.49.0/include", "-L/usr/local/boost-1.49.0/lib", "-Wl,-rpath,/usr/local/boost-1.49.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.49.0",
                "display-flags":"-I/usr/local/boost-1.49.0/include",
            }),
            ("boost-1.50", {
                "flags":["-I/usr/local/boost-1.50.0/include", "-L/usr/local/boost-1.50.0/lib", "-Wl,-rpath,/usr/local/boost-1.50.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_systemv", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.50.0",
                "display-flags":"-I/usr/local/boost-1.50.0/include",
            }),
            ("boost-1.51", {
                "flags":["-I/usr/local/boost-1.51.0/include", "-L/usr/local/boost-1.51.0/lib", "-Wl,-rpath,/usr/local/boost-1.51.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.51.0",
                "display-flags":"-I/usr/local/boost-1.51.0/include",
            }),
            ("boost-1.52", {
                "flags":["-I/usr/local/boost-1.52.0/include", "-L/usr/local/boost-1.52.0/lib", "-Wl,-rpath,/usr/local/boost-1.52.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.52.0",
                "display-flags":"-I/usr/local/boost-1.52.0/include",
            }),
            ("boost-1.53", {
                "flags":["-I/usr/local/boost-1.53.0/include", "-L/usr/local/boost-1.53.0/lib", "-Wl,-rpath,/usr/local/boost-1.53.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_atomic", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph"],
                "display-name":"Boost 1.53.0",
                "display-flags":"-I/usr/local/boost-1.53.0/include",
            }),
            ("boost-1.54", {
                "flags":["-I/usr/local/boost-1.54.0/include", "-L/usr/local/boost-1.54.0/lib", "-Wl,-rpath,/usr/local/boost-1.54.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_atomic", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_coroutine", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_log_setup", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph", "-lboost_log"],
                "display-name":"Boost 1.54.0",
                "display-flags":"-I/usr/local/boost-1.54.0/include",
            }),
            ("boost-1.55", {
                "flags":["-I/usr/local/boost-1.55.0/include", "-L/usr/local/boost-1.55.0/lib", "-Wl,-rpath,/usr/local/boost-1.55.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_atomic", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_coroutine", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_log_setup", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph", "-lboost_log"],
                "display-name":"Boost 1.55.0",
                "display-flags":"-I/usr/local/boost-1.55.0/include",
            }),
            ("boost-1.56", {
                "flags":["-I/usr/local/boost-1.56.0/include", "-L/usr/local/boost-1.56.0/lib", "-Wl,-rpath,/usr/local/boost-1.56.0/lib", "-lboost_wserialization", "-lboost_math_tr1f", "-lboost_math_c99l", "-lboost_math_tr1", "-lboost_locale", "-lboost_wave", "-lboost_regex", "-lboost_prg_exec_monitor", "-lboost_atomic", "-lboost_signals", "-lboost_math_c99f", "-lboost_context", "-lboost_program_options", "-lboost_coroutine", "-lboost_serialization", "-lboost_filesystem", "-lboost_system", "-lboost_timer", "-lboost_log_setup", "-lboost_random", "-lboost_chrono", "-lboost_exception", "-lboost_iostreams", "-lboost_thread", "-lboost_date_time", "-lboost_math_c99", "-lboost_math_tr1l", "-lboost_graph", "-lboost_log"],
                "display-name":"Boost 1.56.0",
                "display-flags":"-I/usr/local/boost-1.56.0/include",
            }),
        ]
        return self.resolve_conflicts(pairs)

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
            "sprout":{
                "flags":["-I/usr/local/sprout"],
                "display-name":"Sprout",
                "display-flags":"-I/usr/local/sprout",
            },
            "warning":{
                "flags":["-Wall", "-Wextra"],
                "display-name":"Warnings",
            },
            "oldgcc-warning":{
                "flags":["-Wall", "-W"],
                "display-name":"Warnings",
            },
            "haskell-warning":{
                "flags":"-Wall",
                "display-name":"Warnings",
            },
            "optimize":{
                "flags":["-O2", "-march=native"],
                "display-name":"Optimization",
            },
            "haskell-optimize":{
                "flags":"-O2",
                "display-name":"Optimization",
            },
            "mono-optimize":{
                "flags":"-optimize",
                "display-name":"Optimization",
            },
            "cpp-verbose":{
                "flags":["-v"],
                "display-name":"Verbose",
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
            self.make_boost(),
            self.make_boost_header(),
            self.make_cxx())

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

    def make_gcc(self):
        SWITCHES_DEFAULT = ["warning", "optimize", "cpp-verbose"]
        SWITCHES_BOOST = ["boost-nothing", "boost-1.47", "boost-1.48", "boost-1.49", "boost-1.50", "boost-1.51", "boost-1.52", "boost-1.53", "boost-1.54", "boost-1.55", "boost-1.56"]
        NAMES = [
            ("gcc-head", {
                "params": {},
                "after": {
                    "display-name":"gcc HEAD",
                    "version-command":["/bin/sh", "-c", "/usr/local/gcc-head/bin/g++ --version | head -1 | cut -d' ' -f3-"],
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.9.1", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.9.0", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.8.2", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.8.1", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.7.3", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11"],
                    "initial-checked":["warning", "gnu++11", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.6.4", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++0x", "gnu++0x"],
                    "initial-checked":["warning", "gnu++0x", "boost-1.56", "sprout"],
                },
            }),
            ("gcc-4.5.4", {
                "params": {},
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["c++98", "gnu++98", "c++0x", "gnu++0x"],
                    "initial-checked":["warning", "gnu++0x", "boost-1.56"],
                },
            }),
            ("gcc-4.4.7", {
                "params": {},
                "after": {
                    "switches":["oldgcc-warning", "optimize", "cpp-verbose"] + SWITCHES_BOOST + ["c++98", "gnu++98", "c++0x", "gnu++0x"],
                    "initial-checked":["oldgcc-warning", "gnu++0x", "boost-1.56"],
                },
            }),
            ("gcc-4.3.6", {
                "params": {},
                "after": {
                    "switches":["oldgcc-warning", "optimize", "cpp-verbose", "c++98", "gnu++98", "c++0x", "gnu++0x"],
                    "initial-checked":["oldgcc-warning", "gnu++0x"],
                },
            })
        ]
        FORMATS = {
            "compile-command":[
                "/usr/local/{name}/bin/g++",
                "-oprog.exe",
                "-Wl,-rpath,/usr/local/{name}/lib64",
                "-lpthread",
                "prog.cc"
            ],
            "version-command":["/usr/local/{name}/bin/g++", "-dumpversion"],
            "display-name":"gcc",
            "display-compile-command":"g++ prog.cc",
            "language":"C++",
            "output-file":"prog.cc",
            "run-command":"./prog.exe",
            "displayable": True,
            "compiler-option-raw":True,
        }

        compilers = self.make_common(NAMES, FORMATS)

        return compilers

    def make_clang(self):
        SWITCHES_DEFAULT = ["warning", "optimize", "cpp-verbose"]
        SWITCHES_BOOST = ["boost-nothing", "boost-1.47", "boost-1.48", "boost-1.49", "boost-1.50", "boost-1.51", "boost-1.52", "boost-1.53", "boost-1.54", "boost-1.55", "boost-1.56"]
        NAMES = [
            ("clang-head", {
                "params": {
                    "llvm": "llvm-head",
                    "libcxx": "libcxx-head",
                },
                "after": {
                    "display-name":"clang HEAD",
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++14", "gnu++14", "c++1z", "gnu++1z"],
                    "initial-checked":["warning", "gnu++1z", "boost-1.56", "sprout"],
                    "compile-command":[
                        "/usr/local/llvm-head/bin/run-clang++.sh",
                        "-oprog.exe",
                        "-stdlib=libc++",
                        "-I/usr/local/libcxx-head/include/c++/v1",
                        "-L/usr/local/libcxx-head/lib",
                        "-Wl,-rpath,/usr/local/libcxx-head/lib",
                        "-nostdinc++",
                        "-lpthread",
                        "prog.cc"
                    ],
                    "version-command":["/bin/sh", "-c", "LD_LIBRARY_PATH=/usr/local/gcc-4.8.2/lib64 /usr/local/llvm-head/bin/clang++ --version | head -1 | cut -d' ' -f3-"],
                },
                "lsupc++": True,
            }),
            ("clang-3.5", {
                "params": {
                    "llvm": "llvm-3.5",
                    "libcxx": "libcxx-3.5",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++14", "gnu++14", "c++1z", "gnu++1z"],
                    "initial-checked":["warning", "gnu++1z", "boost-1.56", "sprout"],
                    "compile-command":[
                        "/usr/local/llvm-3.5/bin/run-clang++.sh",
                        "-oprog.exe",
                        "-stdlib=libc++",
                        "-I/usr/local/libcxx-3.5/include/c++/v1",
                        "-L/usr/local/libcxx-3.5/lib",
                        "-Wl,-rpath,/usr/local/libcxx-3.5/lib",
                        "-nostdinc++",
                        "-lpthread",
                        "prog.cc"
                    ],
                    "version-command":["/bin/sh", "-c", "LD_LIBRARY_PATH=/usr/local/gcc-4.8.2/lib64 /usr/local/llvm-3.5/bin/clang++ --version | head -1 | cut -d' ' -f3-"],
                },
                "lsupc++": True,
            }),
            ("clang-3.4", {
                "params": {
                    "llvm": "llvm-3.4",
                    "libcxx": "libcxx-3.4",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
                "lsupc++": True,
            }),
            ("clang-3.3", {
                "params": {
                    "llvm": "llvm-3.3",
                    "libcxx": "libcxx-3.3",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
                "lsupc++": True,
            }), 
            ("clang-3.2", {
                "params": {
                    "llvm": "llvm-3.2",
                    "libcxx": "libcxx-3.0",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["sprout", "c++98", "gnu++98", "c++11", "gnu++11", "c++1y", "gnu++1y"],
                    "initial-checked":["warning", "gnu++1y", "boost-1.56", "sprout"],
                },
                "lsupc++": False,
            }), 
            ("clang-3.1", {
                "params": {
                    "llvm": "llvm-3.1",
                    "libcxx": "libcxx-3.0",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["c++98", "gnu++98", "c++11", "gnu++11"],
                    "initial-checked":["warning", "gnu++11", "boost-1.56"],
                },
                "lsupc++": False,
            }), 
            ("clang-3.0", {
                "params": {
                    "llvm": "llvm-3.0",
                    "libcxx": "libcxx-3.0",
                },
                "after": {
                    "switches": SWITCHES_DEFAULT + SWITCHES_BOOST + ["c++98", "gnu++98", "c++11", "gnu++11"],
                    "initial-checked":["warning", "gnu++11", "boost-1.56"],
                },
                "lsupc++": False,
            })
        ]
        FORMATS = {
           "display-name":"clang",
           "displayable":True,
           "compiler-option-raw":True,
           "display-compile-command":"clang++ prog.cc -stdlib=libc++",
           "language":"C++",
           "output-file":"prog.cc",
           "run-command":"./prog.exe",
           "compile-command":[
               "/usr/local/{llvm}/bin/clang++",
               "-oprog.exe",
               "-stdlib=libc++",
               "-I/usr/local/{libcxx}/include/c++/v1",
               "-L/usr/local/{libcxx}/lib",
               "-Wl,-rpath,/usr/local/{libcxx}/lib",
               "-nostdinc++",
               "-lpthread",
               "prog.cc"
           ],
           "version-command":["/bin/sh", "-c", "/usr/local/{llvm}/bin/clang++ --version | head -1 | cut -d' ' -f3-"],
        }

        compilers = self.make_common(NAMES, FORMATS)

        for name, dic in zip(NAMES, compilers):
            if name[1]["lsupc++"]:
                dic["compile-command"] += ["-lsupc++"]

        return compilers

    def make_ghc(self):
        NAMES = [
            ("ghc-head", {
                "params": {
                },
                "after": {
                    "display-name":"ghc HEAD",
                },
            }),
            ("ghc-7.8.3", {
                "params": {
                },
                "after": {
                },
            }),
            ("ghc-7.6.3", {
                "params": {
                },
                "after": {
                },
            }),
        ]
        FORMATS = {
            "displayable":True,
            "language":"Haskell",
            "output-file":"prog.hs",
            "compiler-option-raw":True,
            "compile-command":["/usr/local/{name}/bin/ghc", "-o", "prog.exe", "prog.hs"],
            "version-command":["/usr/local/{name}/bin/ghc", "--numeric-version"],
            "switches":["haskell-warning", "haskell-optimize"],
            "initial-checked":["haskell-warning"],
            "display-name":"ghc",
            "display-compile-command":"ghc prog.hs",
            "run-command":["./prog.exe"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_mono(self):
        NAMES = [
            ("mcs-head", {
                "params": {
                    "mono": "mono-head",
                },
                "after": {
                    "display-name":"mcs HEAD",
                },
            }),
            ("mcs-3.2.0", {
                "params": {
                    "mono": "mono-3.2.0",
                },
                "after": {
                },
            }),
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

    def make_erlang(self):
        NAMES = [
           ("erlang-head", {
                "params": {
                },
                "after": {
                    "display-name": "erlang HEAD",
                },
           }),
           ("erlang-maint", {
                "params": {
                },
                "after": {
                    "display-name": "erlang maint",
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.erl",
            "run-command":["/usr/local/{name}/bin/escript", "prog.erl"],
            "display-compile-command":"escript prog.erl",
            "language":"Erlang",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/usr/local/{name}/bin/erl", "-eval", "io:format(\"~s~n\", [erlang:system_info(otp_release)]), halt().", "-noshell"],
        }
        compilers = self.make_common(NAMES, FORMATS)
        return compilers

    def make_elixir(self):
        NAMES = [
           ("elixir-head", {
                "params": {
                },
                "after": {
                    "display-name": "Elixir HEAD",
                },
           }),
        ]
        FORMATS = {
            "displayable":True,
            "output-file":"prog.ex",
            "run-command":["/usr/local/{name}/bin/run.sh", "prog.ex"],
            "display-compile-command":"elixir prog.ex",
            "language":"Elixir",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "version-command":["/bin/bash", "-c", "PATH=/usr/local/erlang-head/bin:$PATH /usr/local/{name}/bin/elixir --version | cut -d' ' -f2"],
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

    def make_default1(self):
        COMPILERS = [{
            "name":"gcc-4.8.2-c",
            "displayable":True,
            "display-name":"gcc",
            "display-compile-command":"gcc prog.c",
            "language":"C",
            "compiler-option-raw":True,
            "compile-command":[
                "/usr/local/gcc-4.8.2/bin/gcc",
                "-oprog.exe",
                "-Wl,-rpath,/usr/local/gcc-4.8.2/lib64",
                "-lpthread",
                "prog.c"
            ],
            "version-command":["/usr/local/gcc-4.8.2/bin/gcc", "-dumpversion"],
            "switches":["warning", "optimize", "cpp-verbose", "c89", "c99", "c11"],
            "initial-checked":["warning", "c11"],
            "output-file":"prog.c",
            "run-command":"./prog.exe"
        },{
            "name":"clang-3.3-c",
            "displayable":True,
            "display-name":"clang",
            "display-compile-command":"clang prog.c",
            "language":"C",
            "compiler-option-raw":True,
            "compile-command":[
                "/usr/local/llvm-3.3/bin/clang",
                "-oprog.exe",
                "-lpthread",
                "prog.c"
            ],
            "version-command":["/bin/sh", "-c", "/usr/local/llvm-3.3/bin/clang --version | head -1 | cut -d' ' -f3-"],
            "switches":["warning", "optimize", "cpp-verbose", "c89", "c99", "c11"],
            "initial-checked":["warning", "c11"],
            "output-file":"prog.c",
            "run-command":"./prog.exe"
        },{
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
        },{
            "name":"gdc-head",
            "displayable":True,
            "display-name":"gdc HEAD",
            "compile-command":[
                "/usr/local/gdc-head/bin/gdc",
                "-oprog.exe",
                "-Wl,-rpath,/usr/local/gdc-head/lib64",
                "prog.d"
            ],
            "compiler-option-raw":True,
            "version-command":["/bin/sh", "-c", "/usr/local/gdc-head/bin/gdc --version | head -1 | cut -d' ' -f3-"],
            "run-command":"./prog.exe",
            "display-compile-command":"gdc prog.d",
            "language":"D",
            "output-file":"prog.d"
        },{
            "name": "rill-head",
            "displayable": True,
            "display-name": "Rill HEAD",
            "compile-command": [
                "/bin/bash",
                "/usr/local/rill-head/bin/run_rill.sh",
                "prog.rill",
                "-o",
                "prog.exe"
            ],
            "compiler-option-raw": True,
            "version-command": ["/bin/cat", "/usr/local/rill-head/bin/version"],
            "run-command": "./prog.exe",
            "display-compile-command": "rillc prog.rill",
            "language": "Rill",
            "output-file": "prog.rill",
        }]
        return COMPILERS

    def make_default2(self):
        COMPILERS = [{
            "name":"rust-head",
            "displayable":True,
            "output-file":"prog.rs",
            "run-command":["./prog"],
            "display-name":"Rust HEAD",
            "display-compile-command":"rustc prog.rs",
            "language":"Rust",
            "runtime-option-raw":True,
            "compile-command":["/usr/local/rust-head/bin/run-rustc.sh", "prog.rs"],
            "version-command":["/bin/sh", "-c", "LD_LIBRARY_PATH=/usr/local/rust-head/lib:$LD_LIBRARY_PATH /usr/local/rust-head/bin/run-rustc.sh --version | head -1 | cut -d' ' -f2-"],
        },{
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
            "name":"lua-5.2.2",
            "displayable":True,
            "output-file":"prog.lua",
            "display-name":"lua",
            "display-compile-command":"lua prog.lua",
            "language":"Lua",
            "runtime-option-raw":True,
            "compile-command":"/bin/true",
            "run-command":["/usr/local/lua-5.2.2/bin/lua", "prog.lua"],
            "version-command":["/bin/sh", "-c", "/usr/local/lua-5.2.2/bin/lua -v | cut -d' ' -f2"],
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
            "name":"java7-openjdk",
            "displayable":True,
            "output-file":"prog.java",
            "display-name":"Java7 OpenJDK",
            "display-compile-command":"javac prog.java",
            "jail-name":"jvm",
            "language":"Java",
            "compiler-option-raw":True,
            "compile-command":["/usr/bin/javac", "prog.java"],
            "run-command":["/bin/bash", "/usr/local/java-run/bin/run.sh"],
            "version-command":["/bin/sh", "-c", "/usr/bin/java -version 2>&1 | head -n1"],
            "switches":[],
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
            self.make_gcc() +
            self.make_clang() +
            self.make_default1() +
            self.make_ghc() +
            self.make_mono() +
            self.make_perl() +
            self.make_python() +
            self.make_ruby() +
            self.make_php() +
            self.make_erlang() +
            self.make_elixir() +
            self.make_node() +
            self.make_spidermonkey() +
            self.make_coffee_script() +
            self.make_sqlite() +
            self.make_scala() +
            self.make_default2()
        )

def make_config():
    return {
        "switches": Switches().make(),
        "compilers": Compilers().make(),
    }

if __name__ == '__main__':
    print json.dumps(make_config(), indent=4)
