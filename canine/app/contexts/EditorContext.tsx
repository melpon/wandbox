import { EditorView } from "@codemirror/basic-setup";
import React, { createContext, useContext } from "react";
import { normalizePath } from "~/utils/normalizePath";

export interface EditorSourceData {
  id: string;
  filename: string | null;
  text?: string;
  view?: EditorView;
}

export type EditorType = "default" | "vim" | "emacs";
export type TabKeyType = "2" | "4" | "8" | "tab";
export type TabWidthType = "2" | "4" | "8";
export type RunningState = "init" | "running" | "completed";

export interface EditorSettingsData {
  opened: boolean;
  editor: EditorType;
  tabKey: TabKeyType;
  tabWidth: TabWidthType;
  smartIndent: boolean;
  setOpened: React.Dispatch<React.SetStateAction<boolean>>;
  setEditor: React.Dispatch<React.SetStateAction<EditorType>>;
  setTabKey: React.Dispatch<React.SetStateAction<TabKeyType>>;
  setTabWidth: React.Dispatch<React.SetStateAction<TabWidthType>>;
  setSmartIndent: React.Dispatch<React.SetStateAction<boolean>>;
}

export interface EditorContextState {
  title: string;
  description: string;
  currentTab: number;
  sources: EditorSourceData[];
  stdin: string;
  stdinOpened: boolean;
  settings: EditorSettingsData;
  running: boolean;
  sharable: boolean;

  setTitle: React.Dispatch<React.SetStateAction<string>>;
  setDescription: React.Dispatch<React.SetStateAction<string>>;
  setCurrentTab: React.Dispatch<React.SetStateAction<number>>;
  addSource: (filename: string, text?: string) => number;
  removeSource: (tab: number) => void;
  initSources: (sources: EditorSourceData[]) => void;
  getSourceText: (tab: number) => string;
  setFilename: (tab: number, filename: string) => void;
  setView: (tab: number, view: EditorView) => void;
  setStdin: React.Dispatch<React.SetStateAction<string>>;
  setStdinOpened: React.Dispatch<React.SetStateAction<boolean>>;
  setRunning: React.Dispatch<React.SetStateAction<boolean>>;
  setSharable: React.Dispatch<React.SetStateAction<boolean>>;
}

function useSettings(): EditorSettingsData {
  const [opened, setOpened] = React.useState(false);
  const [editor, setEditor] = React.useState<EditorType>("default");
  const [tabKey, setTabKey] = React.useState<TabKeyType>("4");
  const [tabWidth, setTabWidth] = React.useState<TabWidthType>("4");
  const [smartIndent, setSmartIndent] = React.useState(true);

  return React.useMemo(
    (): EditorSettingsData => ({
      opened,
      editor,
      tabKey,
      tabWidth,
      smartIndent,
      setOpened,
      setEditor,
      setTabKey,
      setTabWidth,
      setSmartIndent,
    }),
    [
      opened,
      editor,
      tabKey,
      tabWidth,
      smartIndent,
      setOpened,
      setEditor,
      setTabKey,
      setTabWidth,
      setSmartIndent,
    ]
  );
}

function useEditorContextState(): EditorContextState {
  const [title, setTitle] = React.useState<string>("");
  const [description, setDescription] = React.useState<string>("");
  const [currentTab, setCurrentTab] = React.useState<number>(0);
  const [sources, setSources] = React.useState<EditorSourceData[]>([
    { id: "wb-editor-main", filename: null },
  ]);
  const [stdin, setStdin] = React.useState<string>("");
  const [stdinOpened, setStdinOpened] = React.useState<boolean>(false);
  const [counter, setCounter] = React.useState<number>(0);
  const settings = useSettings();
  const [running, setRunning] = React.useState<boolean>(false);
  const [sharable, setSharable] = React.useState<boolean>(false);

  const addSource = React.useCallback(
    (filename: string, text?: string): number => {
      // 既に存在している名前だったら、後ろに -1 や -2 を付けて重複させないようにする
      let resolvedFilename = filename;
      let n = 1;
      while (
        sources.findIndex(
          (source): boolean => resolvedFilename === source.filename
        ) >= 0
      ) {
        resolvedFilename = `${filename}-${n}`;
        n += 1;
      }
      const newSources = [
        ...sources,
        {
          id: `wb-editor-tab${counter}`,
          filename: resolvedFilename,
          text: text,
        },
      ];
      setSources(newSources);
      setCounter(counter + 1);
      console.log("addSource", newSources);
      return sources.length;
    },
    [sources, counter]
  );
  const removeSource = React.useCallback(
    (tab: number): void => {
      // デフォルトタブは消せないようにする
      if (tab === 0) {
        return;
      }

      const newSources = [...sources];
      newSources.splice(tab, 1);
      setSources(newSources);
      console.log("removeSource", newSources);
      // 最後のタブだったらカレントを調整する
      if (currentTab === sources.length - 1) {
        setCurrentTab(currentTab - 1);
      }
    },
    [currentTab, sources]
  );

  const getSourceText = React.useCallback(
    (tab: number): string => {
      const s = sources[tab];
      console.log("getSourceText", s.view?.state?.doc?.toString(), s.text);
      return s.text !== undefined ? s.text : s.view!.state.doc.toString();
    },
    [sources]
  );

  const setFilename = React.useCallback(
    (tab: number, filename: string): void => {
      // デフォルトタブは名前変更できない
      if (tab === 0) {
        return;
      }
      // ファイル名の ../ とか ./ みたいなのを normalize する
      const newFilename = normalizePath(filename);
      // 既存のファイルが既にあったら変更させない
      if (
        sources.findIndex(
          (source): boolean => source.filename === newFilename
        ) !== -1
      ) {
        return;
      }

      const newSources = [...sources];
      newSources[tab] = { ...newSources[tab], filename: newFilename };
      setSources(newSources);
      console.log("setFilename", newSources);
    },
    [sources]
  );
  const setView = React.useCallback((tab: number, view: EditorView) => {
    setSources((sources) => {
      if (sources[tab].text !== undefined) {
        view.dispatch({
          changes: {
            from: 0,
            to: view.state.doc.length,
            insert: sources[tab].text,
          },
        });
      }

      const newSources = [...sources];
      newSources[tab] = { ...newSources[tab], view: view, text: undefined };
      return newSources;
    });
  }, []);

  const initSources = React.useCallback((sources: EditorSourceData[]) => {
    let counter = 0;
    const newSources = [];
    for (const source of sources) {
      const id =
        source.filename === null ? "wb-editor-main" : `wb-editor-tab${counter}`;
      newSources.push({
        id: id,
        filename: source.filename,
        text: source.text,
      });
      counter += 1;
    }
    setSources(newSources);
    setCounter(counter);
    console.log("initSources", newSources);
  }, []);

  return {
    title,
    description,
    currentTab,
    sources,
    stdin,
    stdinOpened,
    settings,
    running,
    sharable,
    setTitle,
    setDescription,
    setCurrentTab,
    addSource,
    removeSource,
    initSources,
    getSourceText,
    setFilename,
    setView,
    setStdin,
    setStdinOpened,
    setRunning,
    setSharable,
  };
}

const EditorContext = createContext<EditorContextState | null>(null);

function useEditorContext(): EditorContextState {
  return useContext(EditorContext) as EditorContextState;
}

export { EditorContext, useEditorContextState, useEditorContext };
