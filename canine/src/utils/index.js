// @flow

import './mode-pony'

export function normalizePath(path: string): string {
  const parts = path.split('/')
  const result = []
  for (let i = 0; i < parts.length; i++) {
    const part = parts[i]
    if (part == '') continue
    if (part == '.') continue
    if (part == '..') {
      if (result.length != 0) {
        result.pop()
      }
      continue
    }
    result.push(part)
  }
  return result.join('/')
}

const LANGUAGE_MODE = [
  {
    language: 'Bash script',
    extensions: ['sh'],
    mode: 'text/x-sh'
  },
  {
    language: 'C',
    extensions: ['h', 'c'],
    mode: 'text/x-csrc'
  },
  {
    language: 'C++',
    extensions: ['h', 'hpp', 'cc', 'cpp', 'cxx'],
    mode: 'text/x-c++src'
  },
  {
    language: 'C#',
    extensions: ['cs'],
    mode: 'text/x-csharp'
  },
  {
    language: 'CPP',
    extensions: ['c', 'cpp'],
    mode: 'text/x-csrc'
  },
  {
    language: 'CoffeeScript',
    extensions: ['coffee'],
    mode: 'text/x-coffeescript'
  },
  {
    language: 'Crystal',
    extensions: ['cr'],
    mode: 'text/x-crystal'
  },
  {
    language: 'D',
    extensions: ['d'],
    mode: 'text/x-d'
  },
  {
    language: 'Elixir',
    extensions: ['ex', 'exs'],
    mode: 'text/x-elixir'
  },
  {
    language: 'Erlang',
    extensions: ['erl'],
    mode: 'text/x-erlang'
  },
  {
    language: 'F#',
    extensions: ['fs'],
    mode: 'text/x-fsharp'
  },
  {
    language: 'Go',
    extensions: ['go'],
    mode: 'text/x-go'
  },
  {
    language: 'Groovy',
    extensions: ['groovy'],
    mode: 'text/x-groovy'
  },
  {
    language: 'Haskell',
    extensions: ['hs'],
    mode: 'text/x-haskell'
  },
  {
    language: 'Java',
    extensions: ['java'],
    mode: 'text/x-java'
  },
  {
    language: 'JavaScript',
    extensions: ['js'],
    mode: 'text/javascript'
  },
  {
    language: 'Lisp',
    extensions: ['lisp', 'cl', 'el'],
    mode: 'text/x-common-lisp'
  },
  {
    language: 'Lua',
    extensions: ['lua'],
    mode: 'text/x-lua'
  },
  {
    language: 'OCaml',
    extensions: ['ml'],
    mode: 'text/x-ocaml'
  },
  {
    language: 'OpenSSL',
    extensions: [],
    mode: 'text/x-sh'
  },
  {
    language: 'PHP',
    extensions: ['php'],
    mode: 'text/x-php'
  },
  {
    language: 'Pascal',
    extensions: ['pas'],
    mode: 'text/x-pascal'
  },
  {
    language: 'Perl',
    extensions: ['pl'],
    mode: 'text/x-perl'
  },
  {
    language: 'Pony',
    extensions: ['pony'],
    mode: 'text/x-pony'
  },
  {
    language: 'Python',
    extensions: ['py'],
    mode: 'text/x-python'
  },
  {
    language: 'Ruby',
    extensions: ['rb'],
    mode: 'text/x-ruby'
  },
  {
    language: 'Rust',
    extensions: ['rs'],
    mode: 'text/x-rustsrc'
  },
  {
    language: 'Scala',
    extensions: ['scala'],
    mode: 'text/x-scala'
  },
  {
    language: 'Swift',
    extensions: ['swift'],
    mode: 'text/x-swift'
  },
  {
    language: 'Vim script',
    extensions: ['vim'],
    mode: 'text/x-csrc'
  }
]

export function resolveLanguageMode(
  filename: string | null,
  language: string,
  fallback: string
): string {
  if (filename !== null) {
    const xs = filename.split('.')
    if (xs.length >= 2) {
      const extension = xs[xs.length - 1]
      for (const lm of LANGUAGE_MODE) {
        for (const ext of lm.extensions) {
          if (ext == extension) {
            return lm.mode
          }
        }
      }
    }
  }

  const lm = LANGUAGE_MODE.find(lm => lm.language == language)
  if (lm !== undefined) {
    return lm.mode
  }

  return fallback
}
