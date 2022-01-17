import React, { useLayoutEffect, useRef, useState } from "react";
import {
  EditorView,
  keymap,
  highlightSpecialChars,
  drawSelection,
  highlightActiveLine,
  KeyBinding,
} from "@codemirror/view";
import {
  Extension,
  EditorState,
  StateEffect,
  Transaction,
  StateCommand,
} from "@codemirror/state";
import { history, historyKeymap } from "@codemirror/history";
import { foldGutter, foldKeymap } from "@codemirror/fold";
import { indentOnInput, getIndentUnit, indentUnit } from "@codemirror/language";
import { lineNumbers, highlightActiveLineGutter } from "@codemirror/gutter";
import { defaultKeymap, defaultTabBinding } from "@codemirror/commands";
import { bracketMatching } from "@codemirror/matchbrackets";
import { closeBrackets, closeBracketsKeymap } from "@codemirror/closebrackets";
import { searchKeymap, highlightSelectionMatches } from "@codemirror/search";
import { autocompletion, completionKeymap } from "@codemirror/autocomplete";
import { commentKeymap } from "@codemirror/comment";
import { rectangularSelection } from "@codemirror/rectangular-selection";
import { defaultHighlightStyle } from "@codemirror/highlight";
import { lintKeymap } from "@codemirror/lint";
import {
  oneDarkTheme,
  oneDarkHighlightStyle,
} from "@codemirror/theme-one-dark";
import { cpp } from "@codemirror/lang-cpp";
import { useEffect } from "react";

export const insertTabWithSpace: StateCommand = ({ state, dispatch }) => {
  const cursor =
    state.selection.main.head -
    state.doc.lineAt(state.selection.main.head).from;
  const indentUnit = getIndentUnit(state);
  const newCursor = Math.floor((cursor + indentUnit) / indentUnit) * indentUnit;
  const indentNum = newCursor - cursor;
  const spaces = Array(indentNum + 1).join(" ");
  console.log(cursor, newCursor, indentUnit, spaces.length);
  dispatch(
    state.update(state.replaceSelection(spaces), {
      scrollIntoView: true,
      annotations: Transaction.userEvent.of("input"),
    })
  );
  return true;
};

export const tabWithSpaceBinding: KeyBinding = {
  key: "Tab",
  run: insertTabWithSpace,
};

const codeMirrorDefaultExtensions: Extension[] = [
  lineNumbers(),
  highlightActiveLineGutter(),
  highlightSpecialChars({ replaceTabs: true }),
  history(),
  foldGutter(),
  drawSelection(),
  EditorState.allowMultipleSelections.of(true),
  indentOnInput(),
  defaultHighlightStyle.fallback,
  bracketMatching(),
  closeBrackets(),
  autocompletion(),
  rectangularSelection(),
  highlightActiveLine(),
  highlightSelectionMatches(),
  //oneDarkTheme,
  //oneDarkHighlightStyle,
  //cpp(),
];

const keymaps: Extension = keymap.of([
  ...closeBracketsKeymap,
  ...defaultKeymap,
  ...searchKeymap,
  ...historyKeymap,
  ...foldKeymap,
  ...commentKeymap,
  ...completionKeymap,
  ...lintKeymap,
]);

export interface CodeMirror6Option {
  keyMap?: string;
  lineNumbers?: boolean;
  theme?: string;
  mode?: string;
  indentUnit?: number;
  indentWithTabs?: boolean;
  tabSize?: number;
  smartIndent?: boolean;
  extraKeys?: { [key: string]: (view: EditorView) => void };
  viewportMargin?: number;
  readOnly?: boolean;
}

export interface CodeMirror6Props {
  className?: string;
  initialText: string;
  option: CodeMirror6Option;
  onViewCreated: (view: EditorView) => void;
}

function optionToExtension(option: CodeMirror6Option): Extension[] {
  const ext = [...codeMirrorDefaultExtensions];
  if (option.readOnly) {
    ext.push(keymaps);
  }
  if (option.tabSize !== undefined) {
    ext.push(EditorState.tabSize.of(option.tabSize));
  }
  if (option.indentUnit !== undefined) {
    ext.push(indentUnit.of(Array(option.indentUnit + 1).join(" ")));
  }
  if (!option.readOnly) {
    if (option.indentWithTabs) {
      ext.push(keymap.of([defaultTabBinding]));
    } else {
      ext.push(keymap.of([tabWithSpaceBinding]));
    }
  }
  return ext;
}

const CodeMirror6 = (props: CodeMirror6Props): React.ReactElement => {
  const { className, initialText, option, onViewCreated } = props;

  const ref = useRef<HTMLDivElement>(null);
  const [view, setView] = useState<EditorView | null>(null);

  useLayoutEffect(() => {
    const startState = EditorState.create({
      doc: initialText,
      extensions: optionToExtension(option),
    });

    const view = new EditorView({
      state: startState,
      parent: ref.current || undefined,
    });

    setView(view);
    onViewCreated(view);

    return () => {
      view.destroy();
      setView(null);
    };
  }, [ref]);

  useEffect(() => {
    if (view === null) {
      return;
    }
    view.dispatch({
      effects: StateEffect.reconfigure.of(optionToExtension(option)),
    });
  }, [option]);

  return <div className={className} ref={ref}></div>;
};

export { CodeMirror6 };
