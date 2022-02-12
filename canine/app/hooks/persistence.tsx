import React, { useMemo } from "react";
import { EditorContextState, EditorSourceData } from "~/contexts/EditorContext";
import { CompilerContextState } from "~/contexts/CompilerContext";
import { ResultContextState, ResultData } from "~/contexts/ResultContext";
import { EditorView } from "@codemirror/view";
import { SidebarContextState } from "~/contexts/SidebarContext";

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
  compiler: CompilerContextState,
  result: ResultContextState,
  sidebar: SidebarContextState,
  initialPermlinkMode: boolean
): Persistence {
  // 最初に開いたページが permlink だった場合、
  // COMPILER_KEY, EDITOR_CODE_KEY はセーブ・ロードしない。
  // そのために、初回で渡された permlinkMode を覚えておいて、それを利用する
  const permlinkMode = useMemo(() => initialPermlinkMode, []);

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
        compiler.setCurrentLanguage(map.currentLanguage);
      }
      if (map.currentCompilerName !== undefined) {
        compiler.setCurrentCompilerName(map.currentCompilerName);
      }
      if (map.currentSwitches !== undefined) {
        compiler.setCurrentSwitches(map.currentSwitches);
      }
      if (map.compilerOptionRaw !== undefined) {
        compiler.setCompilerOptionRaw(map.compilerOptionRaw);
      }
      if (map.runtimeOptionRaw !== undefined) {
        compiler.setRuntimeOptionRaw(map.runtimeOptionRaw);
      }
    }

    // RESULT_RESULTS_KEY のロード
    if (!permlinkMode) {
      const item = localStorage.getItem(RESULT_RESULTS_KEY) || "[]";
      const ar = JSON.parse(item);
      result.setResults(ar as ResultData[]);
    }

    // SIDEBAR_KEY のロード
    const item = JSON.parse(localStorage.getItem(SIDEBAR_KEY) || "{}");
    sidebar.setLocked(item.locked || false);
    sidebar.setOpened(item.opened || false);
  }, [editor, compiler, result, sidebar]);

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
        currentLanguage: compiler.currentLanguage,
        currentCompilerName: compiler.currentCompilerName,
        currentSwitches: compiler.currentSwitches,
        compilerOptionRaw: compiler.compilerOptionRaw,
        runtimeOptionRaw: compiler.runtimeOptionRaw,
      };
      localStorage.setItem(COMPILER_KEY, JSON.stringify(item));
    }
    if (!permlinkMode) {
      localStorage.setItem(RESULT_RESULTS_KEY, JSON.stringify(result.results));
    }
    localStorage.setItem(
      SIDEBAR_KEY,
      JSON.stringify({
        locked: sidebar.locked,
        opened: sidebar.opened,
      })
    );
  }, [editor, compiler, result, sidebar]);

  return {
    save,
    load,
  };
}
