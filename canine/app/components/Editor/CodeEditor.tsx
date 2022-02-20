import React, { useCallback, useEffect, useMemo } from "react";
import { useSelector } from "react-redux";
import { KeyBinding } from "@codemirror/view";

import { resolveLanguage, importLanguage } from "~/utils/resolveLanguageMode";
import { CompilerList } from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { useCompile } from "~/hooks/compile";
import { CodeMirror6, CodeMirror6Option } from "../CodeMirror6";
import { Extension } from "@codemirror/state";
import { EditorSourceData, wandboxSlice, WandboxState } from "~/features/slice";
import { AppState, useAppDispatch } from "~/store";
import { useCompileStateSelector } from "~/utils/compile";

interface CodeEditorProps {
  source: EditorSourceData;
  tab: number;
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const CodeEditor: React.FC<CodeEditorProps> = (props): React.ReactElement => {
  const { source, tab, compilerList, permlinkData } = props;

  const { currentLanguage, stdinOpened, currentTab, tabKey, tabWidth } =
    useSelector(
      ({
        wandbox: {
          currentLanguage,
          stdinOpened,
          currentTab,
          editorSettings: { tabKey, tabWidth },
        },
      }: AppState) => ({
        currentLanguage,
        stdinOpened,
        currentTab,
        tabKey,
        tabWidth,
      })
    );
  const show = tab === currentTab;

  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  const compileState = useCompileStateSelector();
  const doCompile = useCompile(dispatch, compileState, compilerList);
  const onRun = useCallback(() => {
    dispatch(actions.setRunning(true));
    dispatch(actions.setSharable(true));
    dispatch(actions.prepareRun());
    doCompile();
  }, [doCompile]);
  //const { currentLanguage, currentCompilerName } = compiler;

  /*
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
      className="wb-editor"
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
  */
  const language =
    permlinkData === null
      ? currentLanguage
      : permlinkData.parameter.compilerInfo.language;

  const [languageSupport, setLanguageSupport] =
    React.useState<Extension | null>(null);
  const resolvedLanguage = resolveLanguage(source.filename, language);
  useEffect((): void => {
    // mode に応じて動的インポート
    importLanguage(resolvedLanguage).then((lang) => {
      // 読み込みが完了したら CodeEditor をリフレッシュ
      setLanguageSupport(lang);
    });
  }, [resolvedLanguage]);

  const ctrlEnter = useMemo((): KeyBinding => {
    return {
      key: "Ctrl-Enter",
      preventDefault: true,
      run: () => {
        console.log("run");
        onRun();
        return true;
      },
    };
  }, [onRun]);
  const option = useMemo((): CodeMirror6Option => {
    return {
      lineNumbers: true,
      tabSize: parseInt(tabWidth, 10),
      indentUnit: tabKey !== "tab" ? parseInt(tabKey, 10) : undefined,
      indentWithTab: tabKey === "tab",
      languageSupport: languageSupport || undefined,
      readOnly: permlinkData !== null,
      keymaps: [ctrlEnter],
    };
  }, [tabWidth, tabKey, languageSupport, permlinkData, ctrlEnter]);

  return (
    <CodeMirror6
      className={`wb-editor flex-grow-1 ${
        stdinOpened ? "wb-stdinactive" : ""
      } ${show ? "" : "d-none"}`}
      text={source.text}
      option={option}
      onViewCreated={(view) => {
        if (permlinkData === null) {
          dispatch(actions.setView({ tab, view }));
        }
      }}
      onViewDestroyed={() => {}}
      onChange={() => {
        dispatch(actions.setSharable(false));
        dispatch(actions.setEditorChanged(true));
      }}
    />
  );
};

export { CodeEditor };
