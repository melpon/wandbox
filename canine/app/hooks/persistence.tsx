import React, { useMemo } from "react";
import { EditorView } from "@codemirror/view";
import {
  EditorSourceData,
  ResultData,
  wandboxSlice,
  WandboxState,
} from "~/features/slice";
import { AppDispatch, useAppDispatch } from "~/store";

interface Persistence {
  load: () => void;
  save: () => void;
}

const EDITOR_CODE_KEY = "wandbox.editor.code";
const EDITOR_SETTINGS_KEY = "wandbox.editor.settings";
const RESULT_RESULTS_KEY = "wandbox.result.results";
const COMPILER_KEY = "wandbox.compiler";
const SIDEBAR_KEY = "wandbox.sidebar";

export function usePersistence(
  dispatch: AppDispatch,
  state: WandboxState,
  initialPermlinkMode: boolean
): Persistence {
  // 最初に開いたページが permlink だった場合、
  // COMPILER_KEY, EDITOR_CODE_KEY はセーブ・ロードしない。
  // そのために、初回で渡された permlinkMode を覚えておいて、それを利用する
  const permlinkMode = useMemo(() => initialPermlinkMode, []);
  const actions = wandboxSlice.actions;

  // 以前の状態のロード
  const load = React.useCallback((): void => {
    // EDITOR_CODE_KEY のロード
    if (!permlinkMode) {
      const item = localStorage.getItem(EDITOR_CODE_KEY) || "{}";
      const map = JSON.parse(item);
      if (map.sources !== undefined) {
        dispatch(actions.initSources(map.sources as EditorSourceData[]));
      }
      if (map.currentTab !== undefined) {
        dispatch(actions.setCurrentTab(map.currentTab));
      }
      if (map.stdin !== undefined) {
        dispatch(actions.setStdin(map.stdin));
      }
      if (map.title !== undefined) {
        dispatch(actions.setTitle(map.title));
      }
      if (map.description !== undefined) {
        dispatch(actions.setDescription(map.description));
      }
    }
    // EDITOR_SETTINGS_KEY のロード
    {
      const item = localStorage.getItem(EDITOR_SETTINGS_KEY) || "{}";
      const map = JSON.parse(item);
      if (map.editor !== undefined) {
        dispatch(actions.setEditorSettingsEditor(map.editor));
      }
      if (map.opened !== undefined) {
        dispatch(actions.setEditorSettingsOpened(map.opened));
      }
      if (map.smartIndent !== undefined) {
        dispatch(actions.setEditorSettingsSmartIndent(map.smartIndent));
      }
      if (map.tabKey !== undefined) {
        dispatch(actions.setEditorSettingsTabKey(map.tabKey));
      }
      if (map.tabWidth !== undefined) {
        dispatch(actions.setEditorSettingsTabWidth(map.tabWidth));
      }
    }
    // COMPILER_KEY のロード
    if (!permlinkMode) {
      const item = localStorage.getItem(COMPILER_KEY) || "{}";
      const map = JSON.parse(item);
      if (map.currentLanguage !== undefined) {
        dispatch(actions.setCurrentLanguage(map.currentLanguage));
      }
      if (map.currentCompilerName !== undefined) {
        dispatch(actions.setCurrentCompilerName(map.currentCompilerName));
      }
      if (map.currentSwitches !== undefined) {
        dispatch(actions.setCurrentSwitches(map.currentSwitches));
      }
      if (map.compilerOptionRaw !== undefined) {
        dispatch(actions.setCompilerOptionRaw(map.compilerOptionRaw));
      }
      if (map.runtimeOptionRaw !== undefined) {
        dispatch(actions.setRuntimeOptionRaw(map.runtimeOptionRaw));
      }
    }

    // RESULT_RESULTS_KEY のロード
    if (!permlinkMode) {
      const item = localStorage.getItem(RESULT_RESULTS_KEY) || "[]";
      const ar = JSON.parse(item);
      dispatch(actions.setResults(ar as ResultData[]));
    }

    // SIDEBAR_KEY のロード
    const item = JSON.parse(localStorage.getItem(SIDEBAR_KEY) || "{}");
    dispatch(actions.setHistoryLocked(item.locked || false));
    dispatch(actions.setHistoryOpened(item.opened || false));
  }, [state]);

  // セーブ
  const save = React.useCallback((): void => {
    if (!permlinkMode) {
      // view の中身を text に移動
      let hasError = false;
      const sources = state.sources.map((s) => {
        if (s.view === undefined && s.text === undefined) {
          hasError = true;
          return;
        }
        console.log(s);
        return {
          ...s,
          view: undefined,
          text: s.text !== undefined ? s.text : s.view!.state.doc.toString(),
        };
      });
      if (!hasError) {
        const item = {
          sources: sources,
          currentTab: state.currentTab,
          stdin: state.stdin,
          title: state.title,
          description: state.description,
        };
        localStorage.setItem(EDITOR_CODE_KEY, JSON.stringify(item));
      }
    }
    {
      const item = {
        editor: state.editorSettings.editor,
        opened: state.editorSettings.opened,
        smartIndent: state.editorSettings.smartIndent,
        tabKey: state.editorSettings.tabKey,
        tabWidth: state.editorSettings.tabWidth,
      };
      localStorage.setItem(EDITOR_SETTINGS_KEY, JSON.stringify(item));
    }
    if (!permlinkMode) {
      const item = {
        currentLanguage: state.currentLanguage,
        currentCompilerName: state.currentCompilerName,
        currentSwitches: state.currentSwitches,
        compilerOptionRaw: state.compilerOptionRaw,
        runtimeOptionRaw: state.runtimeOptionRaw,
      };
      localStorage.setItem(COMPILER_KEY, JSON.stringify(item));
    }
    if (!permlinkMode) {
      localStorage.setItem(RESULT_RESULTS_KEY, JSON.stringify(state.results));
    }
    localStorage.setItem(
      SIDEBAR_KEY,
      JSON.stringify({
        locked: state.historyLocked,
        opened: state.historyOpened,
      })
    );
  }, [state]);

  return {
    save,
    load,
  };
}
