import React, { useCallback, useEffect, useMemo, useState } from "react";
import { useSelector } from "react-redux";
import type { KeyBinding } from "@codemirror/view";
import type { Extension } from "@codemirror/state";

import { resolveLanguage, importLanguage } from "~/utils/resolveLanguageMode";
import type { CompilerList } from "~/hooks/compilerList";
import type { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { CodeMirror6 } from "../CodeMirror6";
import type { CodeMirror6Option } from "../CodeMirror6";
import type { EditorSourceData } from "~/features/slice";
import { wandboxSlice } from "~/features/slice";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
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

  const language =
    permlinkData === null
      ? currentLanguage
      : permlinkData.parameter.compilerInfo.language;

  const [languageSupport, setLanguageSupport] = useState<Extension | null>(
    null
  );
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
