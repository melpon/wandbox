import React, { useMemo } from "react";
import { EditorContextState, EditorSourceData } from "~/contexts/EditorContext";
import { EditorView } from "@codemirror/view";
import { SidebarContextState } from "~/contexts/SidebarContext";
import { ResultData, wandboxSlice, WandboxState } from "~/features/slice";
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
  editor: EditorContextState,
  dispatch: AppDispatch,
  state: WandboxState,
  sidebar: SidebarContextState,
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
        editor.initSources(map.sources as EditorSourceData[]);
      }
      if (map.currentTab !== undefined) {
        editor.setCurrentTab(map.currentTab);
      }
      if (map.stdin !== undefined) {
        editor.setStdin(map.stdin);
      }
      if (map.title !== undefined) {
        editor.setTitle(map.title);
      }
      if (map.description !== undefined) {
        editor.setDescription(map.description);
      }
    }
    // EDITOR_SETTINGS_KEY のロード
    {
      const item = localStorage.getItem(EDITOR_SETTINGS_KEY) || "{}";
      const map = JSON.parse(item);
      if (map.editor !== undefined) {
        editor.settings.setEditor(map.editor);
      }
      if (map.opened !== undefined) {
        editor.settings.setOpened(map.opened);
      }
      if (map.smartIndent !== undefined) {
        editor.settings.setSmartIndent(map.smartIndent);
      }
      if (map.tabKey !== undefined) {
        editor.settings.setTabKey(map.tabKey);
      }
      if (map.tabWidth !== undefined) {
        editor.settings.setTabWidth(map.tabWidth);
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
    sidebar.setLocked(item.locked || false);
    sidebar.setOpened(item.opened || false);
  }, [editor, state, sidebar]);

  // セーブ
  const save = React.useCallback((): void => {
    if (!permlinkMode) {
      // view の中身を text に移動
      let hasError = false;
      const sources = editor.sources.map((s) => {
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
          currentTab: editor.currentTab,
          stdin: editor.stdin,
          title: editor.title,
          description: editor.description,
        };
        localStorage.setItem(EDITOR_CODE_KEY, JSON.stringify(item));
      }
    }
    {
      const item = {
        editor: editor.settings.editor,
        opened: editor.settings.opened,
        smartIndent: editor.settings.smartIndent,
        tabKey: editor.settings.tabKey,
        tabWidth: editor.settings.tabWidth,
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
        locked: sidebar.locked,
        opened: sidebar.opened,
      })
    );
  }, [editor, state.currentLanguage, sidebar]);

  return {
    save,
    load,
  };
}
