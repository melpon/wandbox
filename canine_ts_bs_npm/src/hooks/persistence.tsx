import React from "react";
import { EditorContextState } from "~/contexts/EditorContext";
import { CompilerContextState } from "~/contexts/CompilerContext";
import { ResultContextState } from "~/contexts/ResultContext";

interface Persistence {
  load: () => void;
  save: () => void;
}

const EDITOR_CODE_KEY = "wandbox.editor.code";
const EDITOR_SETTINGS_KEY = "wandbox.editor.settings";
//const RESULT_SETTINGS_KEY = "wandbox.result.settings";
const COMPILER_KEY = "wandbox.compiler";

export function usePersistence(
  editor: EditorContextState,
  compiler: CompilerContextState,
  result: ResultContextState,
  initialPermlinkMode: boolean
): Persistence {
  // 最初に開いたページが permlink だった場合、
  // COMPILER_KEY, EDITOR_CODE_KEY はセーブ・ロードしない。
  // そのために、初回で渡された permlinkMode を覚えておいて、それを利用する
  const [permlinkMode] = React.useState(initialPermlinkMode);

  // 以前の状態のロード
  const load = React.useCallback((): void => {
    // EDITOR_CODE_KEY のロード
    if (!permlinkMode) {
      const item = localStorage.getItem(EDITOR_CODE_KEY) || "{}";
      const map = JSON.parse(item);
      if (map.sources !== undefined) {
        editor.setSources(map.sources);
      }
      if (map.currentTab !== undefined) {
        editor.setCurrentTab(map.currentTab);
      }
      if (map.stdin !== undefined) {
        editor.setStdin(map.stdin);
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
  }, [editor, compiler, result]);

  // セーブ
  const save = React.useCallback((): void => {
    if (!permlinkMode) {
      const item = {
        sources: editor.sources,
        currentTab: editor.currentTab,
        stdin: editor.stdin,
      };
      localStorage.setItem(EDITOR_CODE_KEY, JSON.stringify(item));
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
  }, [editor, compiler, result]);

  return {
    save,
    load,
  };
}
