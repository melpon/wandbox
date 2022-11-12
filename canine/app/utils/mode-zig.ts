// https://github.com/ziglang/vscode-zig/blob/ac25b33a6b3cfd60efbdd6e01459c22e79902af8/syntaxes/zig.tmLanguage.json
// を参考にしながら実装

// @ts-nocheck

import { simpleMode } from "@codemirror/legacy-modes/mode/simple-mode";

export const zig = simpleMode({
  // The start state contains the rules that are initially used
  start: [
    // comments
    { regex: /\/\/.*/, token: "comment" },
    { regex: /\/\*/, token: "comment", next: "comment" },
    {
      regex: /\b(fn)\s+([A-Z][a-zA-Z0-9]*)\b/,
      token: ["keyword", "typeName"],
    },
    {
      regex: /\b(fn)\s+([_a-zA-Z][_a-zA-Z0-9]*)\b/,
      token: ["keyword", "def"],
    },
    {
      regex: /\b(fn)\s+(@".*")/,
      token: ["keyword", "def"],
    },
    {
      regex: /\b(const|var|fn)\b/,
      token: "keyword",
    },
    // constant.numeric.float.zig
    {
      regex: /\b[0-9][0-9_]*(\.[0-9][0-9_]*)?([eE][+-]?[0-9_]+)?\b/,
      token: "number",
    },
    // constant.numeric.decimal.zig
    {
      regex: /\b[0-9][0-9_]*\b/,
      token: "number",
    },
    // constant.numeric.hexadecimal.zig
    {
      regex: /\b0x[a-fA-F0-9_]+\b/,
      token: "number",
    },
    // constant.numeric.octal.zig
    {
      regex: /\b0o[0-7_]+\b/,
      token: "number",
    },
    // constant.numeric.binary.zig
    {
      regex: /\b0b[01_]+\b/,
      token: "number",
    },

    // keywords
    // keyword.control.repeat.zig
    {
      regex: /(\binline\b)?\s*\b(while|for)\b/,
      token: "keyword",
    },
    // keyword.storage.zig
    {
      regex:
        /\b(extern|packed|export|pub|noalias|inline|comptime|volatile|align|linksection|threadlocal|allowzero|noinline|callconv)\b/,
      token: "keyword",
    },
    // keyword.structure.zig
    {
      regex: /\b(struct|enum|union|opaque)\b/,
      token: "keyword",
    },
    // keyword.statement.zig
    {
      regex: /\b(asm|unreachable)\b/,
      token: "keyword",
    },
    // keyword.control.flow.zig
    {
      regex: /\b(break|return|continue|defer|errdefer)\b/,
      token: "keyword",
    },
    // keyword.control.async.zig
    {
      regex: /\b(await|resume|suspend|async|nosuspend)\b/,
      token: "keyword",
    },
    // keyword.control.trycatch.zig
    {
      regex: /\b(try|catch)\b/,
      token: "keyword",
    },
    // keyword.control.conditional.zig
    {
      regex: /\b(if|else|switch|orelse)\b/,
      token: "keyword",
    },
    // keyword.constant.default.zig
    {
      regex: /\b(null|undefined)\b/,
      token: "keyword",
    },
    // keyword.constant.bool.zig
    {
      regex: /\b(true|false)\b/,
      token: "keyword",
    },
    // variable.other.enummember.zig
    {
      regex:
        /[\s\(\[\{](\.[_a-zA-Z][_a-zA-Z0-9]*)(?!\s*=[^>]|\s*\()(?![_a-zA-Z0-9])"/,
      token: ["keyword"],
    },
    {
      regex: /[\s\(\[\{](\.@\"[^\"]*\")(?!\s*=[^>]|\s*\()"/,
      token: ["keyword"],
    },
    // keyword.default.zig
    {
      regex: /\b(usingnamespace|test|and|or)\b/,
      token: "keyword",
    },
    // meta.error-set.zig
    {
      regex: /\b(error)\b\s*{/,
      token: "keyword",
    },
    // keyword.type.zig
    {
      regex:
        /\b(bool|void|noreturn|type|error|anyerror|anyframe|anytype|anyopaque)\b/,
      token: "keyword",
    },
    // keyword.type.integer.zig
    {
      regex:
        /\b(f16|f32|f64|f80|f128|u\d+|i\d+|isize|usize|comptime_int|comptime_float)\b/,
      token: "keyword",
    },
    // keyword.type.c.zig
    {
      regex:
        /\b(c_short|c_ushort|c_int|c_uint|c_long|c_ulong|c_longlong|c_ulonglong|c_longdouble)\b/,
      token: "keyword",
    },

    // keyword.operator.c-pointer.zig
    {
      regex: /\[*c\]/,
      token: "operator",
    },
    // keyword.operator.comparison.zig
    {
      regex: /(\b(and|or)\b)|(==|!=)/,
      token: "operator",
    },
    // keyword.operator.arithmetic.zig
    {
      regex: /(-%?|\+%?|\*%?|\/|%)=?/,
      token: "operator",
    },
    // keyword.operator.bitwise.zig
    {
      regex: /(<<%?|>>|!|&|\^|\|)=?/,
      token: "operator",
    },
    // keyword.operator.special.zig
    {
      regex: /(==|\+\+|\*\*|->)/,
      token: "operator",
    },

    // #string
    { token: "string", regex: /\\\\.*/ },
    { token: "string", regex: /"(?:[^\\]|\\.)*?(?:"|$)/ },
    {
      token: "string",
      regex:
        /'([^'\\]|\\(x\h{2}|[0-2][0-7]{,2}|3[0-6][0-7]?|37[0-7]?|[4-7][0-7]?|.))'/,
    },
  ],
  // The multi-line comment state.
  comment: [
    { token: "comment", regex: /.*?\*\//, next: "start" },
    { token: "comment", regex: /.*/ },
  ],
  // The meta property contains global information about the mode. It
  // can contain properties like lineComment, which are supported by
  // all modes, and also directives like dontIndentStates, which are
  // specific to simple modes.
  meta: {
    dontIndentStates: ["comment"],
    lineComment: "//",
  },
});
