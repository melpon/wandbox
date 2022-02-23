import { Extension } from "@codemirror/state";
import { StreamLanguage } from "@codemirror/stream-parser";

const LANGUAGE_MODE = [
  {
    language: "Bash script",
    extensions: ["sh"],
  },
  {
    language: "C",
    extensions: ["h", "c"],
  },
  {
    language: "C++",
    extensions: ["h", "hpp", "cc", "cpp", "cxx"],
  },
  {
    language: "C#",
    extensions: ["cs"],
  },
  {
    language: "CPP",
    extensions: ["c", "cpp"],
  },
  {
    language: "Crystal",
    extensions: ["cr"],
  },
  {
    language: "D",
    extensions: ["d"],
  },
  {
    language: "Elixir",
    extensions: ["ex", "exs"],
  },
  {
    language: "Erlang",
    extensions: ["erl"],
  },
  {
    language: "Go",
    extensions: ["go"],
  },
  {
    language: "Groovy",
    extensions: ["groovy"],
  },
  {
    language: "Haskell",
    extensions: ["hs"],
  },
  {
    language: "Java",
    extensions: ["java"],
  },
  {
    language: "JavaScript",
    extensions: ["js"],
  },
  {
    language: "Julia",
    extensions: ["jl"],
  },
  {
    language: "Lazy K",
    extensions: [],
  },
  {
    language: "Lisp",
    extensions: ["lisp", "cl", "el"],
  },
  {
    language: "Lua",
    extensions: ["lua"],
  },
  {
    language: "Nim",
    extensions: ["nim"],
  },
  {
    language: "OCaml",
    extensions: ["ml"],
  },
  {
    language: "OpenSSL",
    extensions: [],
  },
  {
    language: "PHP",
    extensions: ["php"],
  },
  {
    language: "Pascal",
    extensions: ["pas"],
  },
  {
    language: "Perl",
    extensions: ["pl"],
  },
  {
    language: "Pony",
    extensions: ["pony"],
  },
  {
    language: "Python",
    extensions: ["py"],
  },
  {
    language: "R",
    extensions: ["r"],
  },
  {
    language: "Ruby",
    extensions: ["rb"],
  },
  {
    language: "Rust",
    extensions: ["rs"],
  },
  {
    language: "SQL",
    extensions: ["sql"],
  },
  {
    language: "Scala",
    extensions: ["scala"],
  },
  {
    language: "Swift",
    extensions: ["swift"],
  },
  {
    language: "TypeScript",
    extensions: ["ts", "tsx"],
  },
  {
    language: "Vim script",
    extensions: ["vim"],
  },
  {
    language: "Plain text",
    extensions: ["txt"],
  },
] as const;

export type Language = typeof LANGUAGE_MODE[number]["language"];

export function resolveLanguage(
  filename: string | null,
  language: string
): Language {
  if (filename !== null) {
    const xs = filename.split(".");
    if (xs.length >= 2) {
      const extension = xs[xs.length - 1];
      for (const lm of LANGUAGE_MODE) {
        for (const ext of lm.extensions) {
          if (ext === extension) {
            return lm.language;
          }
        }
      }
    }
  }

  const lm = LANGUAGE_MODE.find((lm): boolean => lm.language === language);
  if (lm !== undefined) {
    return lm.language;
  }

  return "Plain text";
}

export function importLanguage(language: Language): Promise<Extension | null> {
  switch (language) {
    case "Bash script":
      return import("@codemirror/legacy-modes/mode/shell").then((x) =>
        StreamLanguage.define(x.shell)
      );
    case "C":
      return import("@codemirror/legacy-modes/mode/clike").then((x) =>
        StreamLanguage.define(x.c)
      );
    case "C++":
      return import("@codemirror/lang-cpp").then((x) => x.cpp());
    case "C#":
      return import("@codemirror/legacy-modes/mode/clike").then((x) =>
        StreamLanguage.define(x.csharp)
      );
    case "CPP":
      return import("@codemirror/legacy-modes/mode/clike").then((x) =>
        StreamLanguage.define(x.c)
      );
    case "Crystal":
      return import("@codemirror/legacy-modes/mode/crystal").then((x) =>
        StreamLanguage.define(x.crystal)
      );
    case "D":
      return import("@codemirror/legacy-modes/mode/d").then((x) =>
        StreamLanguage.define(x.d)
      );
    case "Elixir":
      return import("./mode-elixir").then((x) =>
        StreamLanguage.define(x.elixir)
      );
    case "Erlang":
      return import("@codemirror/legacy-modes/mode/erlang").then((x) =>
        StreamLanguage.define(x.erlang)
      );
    case "Go":
      return import("@codemirror/legacy-modes/mode/go").then((x) =>
        StreamLanguage.define(x.go)
      );
    case "Groovy":
      return import("@codemirror/legacy-modes/mode/groovy").then((x) =>
        StreamLanguage.define(x.groovy)
      );
    case "Haskell":
      return import("@codemirror/legacy-modes/mode/haskell").then((x) =>
        StreamLanguage.define(x.haskell)
      );
    case "Java":
      return import("@codemirror/lang-java").then((x) => x.java());
    case "JavaScript":
      return import("@codemirror/lang-javascript").then((x) => x.javascript());
    case "Julia":
      return import("@codemirror/legacy-modes/mode/julia").then((x) =>
        StreamLanguage.define(x.julia)
      );
    case "Lazy K":
      break;
    case "Lisp":
      return import("@codemirror/legacy-modes/mode/commonlisp").then((x) =>
        StreamLanguage.define(x.commonLisp)
      );
    case "Lua":
      return import("@codemirror/legacy-modes/mode/lua").then((x) =>
        StreamLanguage.define(x.lua)
      );
    case "Nim":
      return import("./mode-nim").then((x) => StreamLanguage.define(x.nim));
    case "OCaml":
      return import("@codemirror/legacy-modes/mode/mllike").then((x) =>
        StreamLanguage.define(x.oCaml)
      );
    case "OpenSSL":
      break;
    case "PHP":
      return import("@codemirror/lang-php").then((x) => x.php());
    case "Pascal":
      return import("@codemirror/legacy-modes/mode/pascal").then((x) =>
        StreamLanguage.define(x.pascal)
      );
    case "Perl":
      return import("@codemirror/legacy-modes/mode/perl").then((x) =>
        StreamLanguage.define(x.perl)
      );
    case "Pony":
      return import("./mode-pony").then((x) => StreamLanguage.define(x.pony));
    case "Python":
      return import("@codemirror/lang-python").then((x) => x.python());
    case "R":
      return import("@codemirror/legacy-modes/mode/r").then((x) =>
        StreamLanguage.define(x.r)
      );
    case "Ruby":
      return import("@codemirror/legacy-modes/mode/ruby").then((x) =>
        StreamLanguage.define(x.ruby)
      );
    case "Rust":
      return import("@codemirror/lang-rust").then((x) => x.rust());
    case "SQL":
      return import("@codemirror/legacy-modes/mode/sql").then((x) =>
        StreamLanguage.define(x.sqlite)
      );
    case "Scala":
      return import("@codemirror/legacy-modes/mode/clike").then((x) =>
        StreamLanguage.define(x.scala)
      );
    case "Swift":
      return import("@codemirror/legacy-modes/mode/swift").then((x) =>
        StreamLanguage.define(x.swift)
      );
    case "TypeScript":
      return import("@codemirror/lang-javascript").then((x) =>
        x.javascript({ typescript: true })
      );
    case "Vim script":
      break;
    case "Plain text":
      break;
  }
  return Promise.resolve(null);
}
