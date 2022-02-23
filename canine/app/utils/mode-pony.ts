// https://github.com/ponylang/pony-playground/blob/8a242a799adc3925ed303cdc6648ed47c806136c/static/mode-pony.js
// の ACE 用の設定から CodeMirror6 に対応したもの

// @ts-nocheck

import { simpleMode } from "@codemirror/legacy-modes/mode/simple-mode";

export const pony = simpleMode({
  // The start state contains the rules that are intially used
  start: [
    // comments
    { regex: /\/\/.*/, token: "comment" },
    { regex: /\/\*/, token: "comment", next: "comment" },
    // #constants
    { token: "atom", regex: /\bthis\b/ },
    { token: "atom", regex: /\b(?:true|false)\b/ },
    {
      token: "number",
      regex:
        /\b(?:0b[0-1_]*|0x[0-9a-fA-F_]*|[0-9][0-9_]*(?:\.[0-9][0-9_]*)?(?:(?:e|E)(?:\+|-)?[0-9_]+)?)\b/,
    },
    // #methoddeclarations
    {
      token: ["keyword", null, "keyword", null, "def"],
      regex:
        /\b(new|be|fun)(\s+)((?:iso|trn|ref|val|box|tag)?)\b(\s*)([_a-z][_a-zA-Z0-9]*)/,
    },
    // #typedeclarations
    {
      token: ["keyword", null, "keyword", null, "def"],
      regex:
        /\b(type|interface|trait|primitive|struct|class|actor)(\s+)((?:iso|trn|ref|val|box|tag)?)(@?\s*)([_A-Z][_a-zA-Z0-9]*)/,
    },
    // #identifiers
    { token: ["def", null], regex: /\b([_a-z][_a-zA-Z0-9]*)\b(\(|\[)/ },
    {
      token: [null, "def", null],
      regex: /(\.\s*)([_a-z][_a-zA-Z0-9]*)\b([^\(\[])/,
    },
    {
      token: [null, "def", null, null],
      regex: /(@\s*)([_a-zA-Z][_a-zA-Z0-9]*)(\s*)(\(|\[)/,
    },
    { token: "def", regex: /\b_*[A-Z][_a-zA-Z0-9]*\b/ },
    { token: "variable", regex: /\b_*[a-z][_a-zA-Z0-9']*/ },
    // #keywords
    {
      // keyword.other.intrinsic.pony
      token: "keyword",
      regex: /\b(?:compile_intrinsic|compile_error)\b/,
    },
    {
      // keyword.other.import.pony
      token: "keyword",
      regex: /\buse\b/,
    },
    {
      // keyword.other.declaration.pony
      token: "keyword",
      regex: /\b(?:var|let|embed|delegate)\b/,
    },
    {
      // entity.other.attribute-name.pony
      token: "keyword",
      regex: /\b(?:iso|trn|ref|val|box|tag)\b/,
    },
    {
      // keyword.control.jump.pony
      token: "keyword",
      regex: /\b(?:break|continue|return|error)\b/,
    },
    {
      // keyword.control.pony
      token: "keyword",
      regex:
        /\b(?:if|ifdef|then|elseif|else|end|match|where|try|with|as|recover|consume|object|digestof)\b/,
    },
    {
      // keyword.control.loop.pony
      token: "keyword",
      regex: /\b(?:while|do|repeat|until|for|in)\b/,
    },
    { token: "operator", regex: /\-|\+|\*|\/(?![\/*])|%|<<|>>/ },
    { token: "operator", regex: /==|!=|<=|>=|<|>/ },
    { token: "operator", regex: /\b(?:is|isnt|not|and|or|xor)\b/ },
    { token: "operator", regex: /=/ },
    { token: "operator", regex: /\?|=>/ },
    { token: "operator", regex: /\||\&|\,|\^/ },
    // #string
    { token: "string-2", regex: /"""/, next: "string" },
    { token: "string", regex: /"(?:[^\\]|\\.)*?(?:"|$)/ },
    { token: "string", regex: /'(?:[^\\]|\\.)*?(?:'|$)/ },
  ],
  // The multi-line comment state.
  comment: [
    { token: "comment", regex: /.*?\*\//, next: "start" },
    { token: "comment", regex: /.*/ },
  ],
  string: [
    { token: "string-2", regex: /.*?"""/, next: "start" },
    { token: "string-2", regex: /.*/ },
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
