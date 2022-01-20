const LANGUAGE_MODE = [
  {
    language: "Bash script",
    extensions: ["sh"],
    mode: "text/x-sh",
  },
  {
    language: "C",
    extensions: ["h", "c"],
    mode: "text/x-csrc",
  },
  {
    language: "C++",
    extensions: ["h", "hpp", "cc", "cpp", "cxx"],
    mode: "text/x-c++src",
  },
  {
    language: "C#",
    extensions: ["cs"],
    mode: "text/x-csharp",
  },
  {
    language: "CPP",
    extensions: ["c", "cpp"],
    mode: "text/x-csrc",
  },
  {
    language: "CoffeeScript",
    extensions: ["coffee"],
    mode: "text/x-coffeescript",
  },
  {
    language: "Crystal",
    extensions: ["cr"],
    mode: "text/x-crystal",
  },
  {
    language: "D",
    extensions: ["d"],
    mode: "text/x-d",
  },
  {
    language: "Elixir",
    extensions: ["ex", "exs"],
    mode: "text/x-elixir",
  },
  {
    language: "Erlang",
    extensions: ["erl"],
    mode: "text/x-erlang",
  },
  {
    language: "F#",
    extensions: ["fs"],
    mode: "text/x-fsharp",
  },
  {
    language: "Go",
    extensions: ["go"],
    mode: "text/x-go",
  },
  {
    language: "Groovy",
    extensions: ["groovy"],
    mode: "text/x-groovy",
  },
  {
    language: "Haskell",
    extensions: ["hs"],
    mode: "text/x-haskell",
  },
  {
    language: "Java",
    extensions: ["java"],
    mode: "text/x-java",
  },
  {
    language: "JavaScript",
    extensions: ["js"],
    mode: "text/javascript",
  },
  {
    language: "Lisp",
    extensions: ["lisp", "cl", "el"],
    mode: "text/x-common-lisp",
  },
  {
    language: "Lua",
    extensions: ["lua"],
    mode: "text/x-lua",
  },
  {
    language: "OCaml",
    extensions: ["ml"],
    mode: "text/x-ocaml",
  },
  {
    language: "OpenSSL",
    extensions: [],
    mode: "text/x-sh",
  },
  {
    language: "PHP",
    extensions: ["php"],
    mode: "text/x-php",
  },
  {
    language: "Pascal",
    extensions: ["pas"],
    mode: "text/x-pascal",
  },
  {
    language: "Perl",
    extensions: ["pl"],
    mode: "text/x-perl",
  },
  {
    language: "Pony",
    extensions: ["pony"],
    mode: "text/x-pony",
  },
  {
    language: "Python",
    extensions: ["py"],
    mode: "text/x-python",
  },
  {
    language: "Ruby",
    extensions: ["rb"],
    mode: "text/x-ruby",
  },
  {
    language: "Rust",
    extensions: ["rs"],
    mode: "text/x-rustsrc",
  },
  {
    language: "Scala",
    extensions: ["scala"],
    mode: "text/x-scala",
  },
  {
    language: "Swift",
    extensions: ["swift"],
    mode: "text/x-swift",
  },
  {
    language: "Vim script",
    extensions: ["vim"],
    mode: "text/x-csrc",
  },
];

export function resolveLanguageMode(
  filename: string | null,
  language: string,
  fallback: string
): string {
  if (filename !== null) {
    const xs = filename.split(".");
    if (xs.length >= 2) {
      const extension = xs[xs.length - 1];
      for (const lm of LANGUAGE_MODE) {
        for (const ext of lm.extensions) {
          if (ext === extension) {
            return lm.mode;
          }
        }
      }
    }
  }

  const lm = LANGUAGE_MODE.find((lm): boolean => lm.language === language);
  if (lm !== undefined) {
    return lm.mode;
  }

  return fallback;
}

export function importLanguageMode(mode: string): Promise<unknown> {
  switch (mode) {
    case "text/x-sh":
      return import(
        /* webpackChunkName: "codemirror-mode-shell-shell" */ "codemirror/mode/shell/shell"
      );
    case "text/x-csrc":
      return import(
        /* webpackChunkName: "codemirror-mode-clike-clike" */ "codemirror/mode/clike/clike"
      );
    case "text/x-c++src":
      return import(
        /* webpackChunkName: "codemirror-mode-clike-clike" */ "codemirror/mode/clike/clike"
      );
    case "text/x-csharp":
      return import(
        /* webpackChunkName: "codemirror-mode-clike-clike" */ "codemirror/mode/clike/clike"
      );
    case "text/x-coffeescript":
      return import(
        /* webpackChunkName: "codemirror-mode-coffeescript-coffeescript" */ "codemirror/mode/coffeescript/coffeescript"
      );
    case "text/x-crystal":
      return import(
        /* webpackChunkName: "codemirror-mode-crystal-crystal" */ "codemirror/mode/crystal/crystal"
      );
    case "text/x-d":
      return import(
        /* webpackChunkName: "codemirror-mode-d-d" */ "codemirror/mode/d/d"
      );
    case "text/x-elixir":
      return import(
        /* webpackChunkName: "codemirror-mode-elixir" */ "codemirror-mode-elixir"
      );
    case "text/x-erlang":
      return import(
        /* webpackChunkName: "codemirror-mode-erlang-erlang" */ "codemirror/mode/erlang/erlang"
      );
    case "text/x-fsharp":
      return import(
        /* webpackChunkName: "codemirror-mode-mllike-mllike" */ "codemirror/mode/mllike/mllike"
      );
    case "text/x-go":
      return import(
        /* webpackChunkName: "codemirror-mode-go-go" */ "codemirror/mode/go/go"
      );
    case "text/x-groovy":
      return import(
        /* webpackChunkName: "codemirror-mode-groovy-groovy" */ "codemirror/mode/groovy/groovy"
      );
    case "text/x-haskell":
      return import(
        /* webpackChunkName: "codemirror-mode-haskell-haskell" */ "codemirror/mode/haskell/haskell"
      );
    case "text/x-java":
      return import(
        /* webpackChunkName: "codemirror-mode-clike-clike" */ "codemirror/mode/clike/clike"
      );
    case "text/javascript":
      return import(
        /* webpackChunkName: "codemirror-mode-javascript-javascript" */ "codemirror/mode/javascript/javascript"
      );
    case "text/x-common-lisp":
      return import(
        /* webpackChunkName: "codemirror-mode-commonlisp-commonlisp" */ "codemirror/mode/commonlisp/commonlisp"
      );
    case "text/x-lua":
      return import(
        /* webpackChunkName: "codemirror-mode-lua-lua" */ "codemirror/mode/lua/lua"
      );
    case "text/x-ocaml":
      return import(
        /* webpackChunkName: "codemirror-mode-mllike-mllike" */ "codemirror/mode/mllike/mllike"
      );
    case "text/x-php":
      return import(
        /* webpackChunkName: "codemirror-mode-php-php" */ "codemirror/mode/php/php"
      );
    case "text/x-pascal":
      return import(
        /* webpackChunkName: "codemirror-mode-pascal-pascal" */ "codemirror/mode/pascal/pascal"
      );
    case "text/x-perl":
      return import(
        /* webpackChunkName: "codemirror-mode-perl-perl" */ "codemirror/mode/perl/perl"
      );
    case "text/x-pony":
      return import(
        /* webpackChunkName: "canine-mode-pony" */ "~/utils/mode-pony"
      );
    case "text/x-python":
      return import(
        /* webpackChunkName: "codemirror-mode-python-python" */ "codemirror/mode/python/python"
      );
    case "text/x-ruby":
      return import(
        /* webpackChunkName: "codemirror-mode-ruby-ruby" */ "codemirror/mode/ruby/ruby"
      );
    case "text/x-rustsrc":
      return import(
        /* webpackChunkName: "codemirror-mode-rust-rust" */ "codemirror/mode/rust/rust"
      );
    case "text/x-scala":
      return import(
        /* webpackChunkName: "codemirror-mode-clike-clike" */ "codemirror/mode/clike/clike"
      );
    case "text/x-swift":
      return import(
        /* webpackChunkName: "codemirror-mode-swift-swift" */ "codemirror/mode/swift/swift"
      );
  }
  return Promise.resolve(null);
}
