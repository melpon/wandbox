import { EditorView } from "@codemirror/view";
import { createSlice, PayloadAction } from "@reduxjs/toolkit";
import { normalizePath } from "~/utils/normalizePath";
import { castDraft } from "immer";

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
}

type ResultType =
  | "Control"
  | "CompilerMessageS"
  | "CompilerMessageE"
  | "StdOut"
  | "StdErr"
  | "ExitCode"
  | "Signal";

export interface ResultData {
  type: ResultType;
  data: string;
}

const initialState = {
  currentLanguage: "",
  currentCompilerName: "",
  currentSwitches: {} as { [name: string]: string | boolean },
  compilerOptionRaw: "",
  runtimeOptionRaw: "",

  title: "",
  description: "",

  currentTab: 0,
  tabCounter: 0,
  sources: [] as EditorSourceData[],
  stdin: "",
  stdinOpened: false,
  editorSettings: {
    opened: false,
    editor: "default",
    tabKey: "4",
    tabWidth: "4",
    smartIndent: false,
  } as EditorSettingsData,
  running: false,
  sharable: false,

  results: [] as ResultData[],
};

export type WandboxState = typeof initialState;

export const wandboxSlice = createSlice({
  name: "wandbox",
  initialState: initialState,
  reducers: {
    setCurrentLanguage: (state, action: PayloadAction<string>) => {
      state.currentLanguage = action.payload;
    },
    setCurrentCompilerName: (state, action: PayloadAction<string>) => {
      state.currentCompilerName = action.payload;
    },
    setCurrentSwitch: (
      state,
      action: PayloadAction<{ switchName: string; value: boolean | string }>
    ) => {
      const { switchName, value } = action.payload;
      state.currentSwitches[switchName] = value;
    },
    setCurrentSwitches: (
      state,
      action: PayloadAction<WandboxState["currentSwitches"]>
    ) => {
      state.currentSwitches = action.payload;
    },
    setCompilerOptionRaw: (state, action: PayloadAction<string>) => {
      state.compilerOptionRaw = action.payload;
    },
    setRuntimeOptionRaw: (state, action: PayloadAction<string>) => {
      state.runtimeOptionRaw = action.payload;
    },
    clearResult: (state) => {
      state.results = [];
    },
    addResult: (state, action: PayloadAction<ResultData>) => {
      state.results.push(action.payload);
    },
    setResults: (state, action: PayloadAction<ResultData[]>) => {
      state.results = action.payload;
    },
    setEditorSettingsOpened: (state, action: PayloadAction<boolean>) => {
      state.editorSettings.opened = action.payload;
    },
    setEditorSettingsEditor: (state, action: PayloadAction<EditorType>) => {
      state.editorSettings.editor = action.payload;
    },
    setEditorSettingsTabKey: (state, action: PayloadAction<TabKeyType>) => {
      state.editorSettings.tabKey = action.payload;
    },
    setEditorSettingsTabWidth: (state, action: PayloadAction<TabWidthType>) => {
      state.editorSettings.tabWidth = action.payload;
    },
    setEditorSettingsSmartIndent: (state, action: PayloadAction<boolean>) => {
      state.editorSettings.smartIndent = action.payload;
    },
    setTitle: (state, action: PayloadAction<string>) => {
      state.title = action.payload;
    },
    setDescription: (state, action: PayloadAction<string>) => {
      state.description = action.payload;
    },
    setCurrentTab: (state, action: PayloadAction<number>) => {
      state.currentTab = action.payload;
    },
    addSource: (
      state,
      action: PayloadAction<{ filename: string; text?: string }>
    ) => {
      const { filename, text } = action.payload;
      // 既に存在している名前だったら、後ろに -1 や -2 を付けて重複させないようにする
      let resolvedFilename = filename;
      let n = 1;
      while (
        state.sources.findIndex(
          (source): boolean => resolvedFilename === source.filename
        ) >= 0
      ) {
        resolvedFilename = `${filename}-${n}`;
        n += 1;
      }
      state.sources.push({
        id: `wb-editor-tab${state.tabCounter}`,
        filename: resolvedFilename,
        text: text,
      });
      state.tabCounter += 1;
    },
    removeSource: (state, action: PayloadAction<number>) => {
      const tab = action.payload;
      // デフォルトタブは消せないようにする
      if (action.payload === 0) {
        return;
      }

      // 最後のタブだったらカレントを調整する
      if (state.currentTab === state.sources.length - 1) {
        state.currentTab = state.currentTab - 1;
      }

      state.sources.splice(tab, 1);
    },
    initSources: (state, action: PayloadAction<EditorSourceData[]>) => {
      let counter = 0;
      const newSources = [];
      for (const source of action.payload) {
        const id =
          source.filename === null
            ? "wb-editor-main"
            : `wb-editor-tab${counter}`;
        newSources.push({
          id: id,
          filename: source.filename,
          text: source.text,
        });
        counter += 1;
      }
      state.sources = newSources;
      state.tabCounter = counter;
    },
    setFilename: (
      state,
      action: PayloadAction<{ tab: number; filename: string }>
    ) => {
      const { tab, filename } = action.payload;

      // デフォルトタブは名前変更できない
      if (tab === 0) {
        return;
      }
      // ファイル名の ../ とか ./ みたいなのを normalize する
      const newFilename = normalizePath(filename);
      // 既存のファイルが既にあったら変更させない
      if (
        state.sources.findIndex((source) => source.filename === newFilename) !==
        -1
      ) {
        return;
      }

      state.sources[tab].filename = newFilename;
    },
    setView: (
      state,
      action: PayloadAction<{ tab: number; view: EditorView }>
    ) => {
      const { tab, view } = action.payload;
      state.sources[tab].view = castDraft(view);
      state.sources[tab].text = undefined;
    },
    setStdin: (state, action: PayloadAction<string>) => {
      state.stdin = action.payload;
    },
    setStdinOpened: (state, action: PayloadAction<boolean>) => {
      state.stdinOpened = action.payload;
    },
    setRunning: (state, action: PayloadAction<boolean>) => {
      state.running = action.payload;
    },
    setSharable: (state, action: PayloadAction<boolean>) => {
      state.sharable = action.payload;
    },
  },
  extraReducers: (builder) => {},
});

export const wandboxReducer = wandboxSlice.reducer;
export const wandboxActions = wandboxSlice.actions;
