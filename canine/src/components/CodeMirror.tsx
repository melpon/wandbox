import React from "react";
import type * as codemirror from "codemirror";
import { Controlled as OrigCodeMirror } from "react-codemirror2";

// import "codemirror/lib/codemirror.css";
// import "codemirror/theme/material.css";

import "codemirror/addon/search/searchcursor";
import "codemirror/addon/edit/matchbrackets";
import "codemirror/addon/dialog/dialog";

// eslint-disable-next-line @typescript-eslint/no-var-requires
// const styles = require("./CodeMirror.module.css");

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type CodeMirrorType = any;
export interface CodeMirrorOptions {
  keyMap?: string;
  lineNumbers?: boolean;
  theme?: string;
  mode?: string;
  indentUnit?: number;
  indentWithTabs?: boolean;
  tabSize?: number;
  smartIndent?: boolean;
  extraKeys?: { [key: string]: (cm: CodeMirrorType) => void };
  viewportMargin?: number;
  readOnly?: boolean;
}

export interface CodeMirrorProps {
  value: string;
  options: CodeMirrorOptions;
  onBeforeChange: (
    editor: codemirror.Editor,
    data: codemirror.EditorChange,
    value: string
  ) => void;
  className: "wb-editor" | "wb-input";
}

const CodeMirror = (props: CodeMirrorProps): React.ReactElement => {
  return (
    <OrigCodeMirror
      className={props.className}
      value={props.value}
      options={props.options}
      onBeforeChange={props.onBeforeChange}
    />
  );
};

export { CodeMirror };
