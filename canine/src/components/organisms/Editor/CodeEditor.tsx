import React, { useEffect } from "react";
import {
  CodeMirror,
  CodeMirrorOptions,
  CodeMirrorType,
} from "~/components/organisms/CodeMirror";
import {
  resolveLanguageMode,
  importLanguageMode,
} from "~/utils/resolveLanguageMode";
import { CompilerList } from "~/hooks/compilerList";
import { CompilerContextState } from "~/contexts/CompilerContext";
import { EditorContextState } from "~/contexts/EditorContext";
import { ResultContextState } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { useCompile } from "~/hooks/compile";

interface CodeEditorProps {
  editor: EditorContextState;
  compiler: CompilerContextState;
  compilerList: CompilerList;
  result: ResultContextState;
  permlinkData: PermlinkData | null;
}

const CodeEditor: React.FC<CodeEditorProps> = (props): React.ReactElement => {
  const { editor, compiler, compilerList, result, permlinkData } = props;
  const { currentLanguage, currentCompilerName } = compiler;

  const insertTabSpace = React.useCallback((cm: CodeMirrorType): void => {
    const cursor = cm.getCursor()["ch"];
    const indentUnit = cm.getOption("indentUnit");
    const newCursor =
      Math.floor((cursor + indentUnit) / indentUnit) * indentUnit;
    const indentNum = newCursor - cursor;
    const spaces = Array(indentNum + 1).join(" ");
    cm.replaceSelection(spaces, "end", "+input");
  }, []);

  const doCompile = useCompile(editor, compiler, compilerList, result);

  const onCtrlEnter = React.useCallback((): void => {
    if (permlinkData !== null) {
      return;
    }

    doCompile();
  }, [currentLanguage, currentCompilerName, doCompile]);

  const settings = editor.settings;
  const options = React.useMemo((): CodeMirrorOptions => {
    const options = {
      keyMap: settings.editor,
      smartIndent: settings.smartIndent,
      tabSize: parseInt(settings.tabWidth, 10),
      extraKeys: {
        "Ctrl-Enter": onCtrlEnter,
      },
    };

    if (settings.tabKey === "tab") {
      return {
        ...options,
        indentWithTabs: true,
      };
    } else {
      return {
        ...options,
        extraKeys: {
          ...options.extraKeys,
          Tab: insertTabSpace,
        },
        indentUnit: parseInt(settings.tabKey, 10),
        indentWithTabs: true,
      };
    }
  }, [settings, insertTabSpace, onCtrlEnter]);

  // パーマリンク時は permlinkData からソースデータを作る
  const sources =
    permlinkData === null
      ? editor.sources
      : createEditorSourceData(
          permlinkData.parameter.code,
          permlinkData.parameter.codes
        );

  const source = sources[editor.currentTab];

  const [mode, setMode] = React.useState<string>("text/x-text");
  const tmpMode = resolveLanguageMode(
    source.filename,
    compiler.currentLanguage,
    "text/x-text"
  );
  useEffect((): void => {
    // mode に応じて動的インポート
    importLanguageMode(tmpMode).then((): void => {
      // 読み込みが完了したら CodeEditor をリフレッシュ
      setMode(tmpMode);
    });
  }, [tmpMode]);

  const onBeforeChange = React.useCallback(
    (_editor, _data, value): void => {
      editor.setText(editor.currentTab, value);
    },
    [editor.currentTab]
  );

  return (
    <CodeMirror
      style="editor"
      value={source.text}
      options={{
        readOnly: permlinkData !== null,
        lineNumbers: true,
        theme: "material",
        mode: mode,
        ...options,
      }}
      onBeforeChange={onBeforeChange}
    />
  );
};

export { CodeEditor };
