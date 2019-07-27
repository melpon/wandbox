import React from "react";
import { createContainer } from "unstated-next";

export interface EditorSource {
  filename: string | null;
  text: string;
}

type EditorType = "default" | "vim" | "emacs";
type TabKeyType = "2" | "4" | "8" | "tab";
type TabWidthType = "2" | "4" | "8";

export interface EditorSettings {
  opened: boolean;
  editor: EditorType;
  tabKey: TabKeyType;
  tabWidth: TabWidthType;
  smartIndent: boolean;
  expand: boolean;
  setOpened: React.Dispatch<React.SetStateAction<boolean>>;
  setEditor: React.Dispatch<React.SetStateAction<EditorType>>;
  setTabKey: React.Dispatch<React.SetStateAction<TabKeyType>>;
  setTabWidth: React.Dispatch<React.SetStateAction<TabWidthType>>;
  setSmartIndent: React.Dispatch<React.SetStateAction<boolean>>;
  setExpand: React.Dispatch<React.SetStateAction<boolean>>;
}

export interface EditorContextState {
  currentTab: number;
  sources: EditorSource[];
  stdin: string;
  settings: EditorSettings;

  setCurrentTab: React.Dispatch<React.SetStateAction<number>>;
  addSource: (filename: string) => number;
  removeSource: (tab: number) => void;
  setText: (tab: number, text: string) => void;
  setFilename: (tab: number, filename: string) => void;
  setStdin: React.Dispatch<React.SetStateAction<string>>;
}

function useSettings(): EditorSettings {
  const [opened, setOpened] = React.useState(false);
  const [editor, setEditor] = React.useState<EditorType>("default");
  const [tabKey, setTabKey] = React.useState<TabKeyType>("4");
  const [tabWidth, setTabWidth] = React.useState<TabWidthType>("4");
  const [smartIndent, setSmartIndent] = React.useState(true);
  const [expand, setExpand] = React.useState(false);
  return {
    opened,
    editor,
    tabKey,
    tabWidth,
    smartIndent,
    expand,
    setOpened,
    setEditor,
    setTabKey,
    setTabWidth,
    setSmartIndent,
    setExpand
  };
}

function useEditorContext(): EditorContextState {
  const [currentTab, setCurrentTab] = React.useState<number>(0);
  const [sources, setSources] = React.useState<EditorSource[]>([
    { filename: null, text: "" }
  ]);
  const [stdin, setStdin] = React.useState<string>("");
  const settings = useSettings();

  const addSource = React.useCallback(
    (filename: string): number => {
      const newSources = [...sources, { filename: filename, text: "" }];
      setSources(newSources);
      return sources.length;
    },
    [sources]
  );
  const removeSource = React.useCallback(
    (tab: number): void => {
      // デフォルトタブは消せないようにする
      if (tab === 0) {
        return;
      }

      const newSources = [...sources];
      newSources.splice(currentTab, 1);
      setSources(newSources);
      // 最後のタブだったらカレントを調整する
      if (currentTab === sources.length - 1) {
        setCurrentTab(currentTab - 1);
      }
    },
    [currentTab, sources]
  );
  const setText = React.useCallback(
    (tab: number, text: string): void => {
      const newSources = [...sources];
      newSources[tab] = { ...newSources[tab], text: text };
      setSources(newSources);
    },
    [sources]
  );
  const setFilename = React.useCallback(
    (tab: number, filename: string): void => {
      // デフォルトタブは名前変更できない
      if (tab === 0) {
        return;
      }
      const newSources = [...sources];
      newSources[tab] = { ...newSources[tab], filename: filename };
      setSources(newSources);
    },
    [sources]
  );

  return {
    currentTab,
    sources,
    stdin,
    settings,
    setCurrentTab,
    addSource,
    removeSource,
    setText,
    setFilename,
    setStdin
  };
}

export const EditorContext = createContainer(useEditorContext);
