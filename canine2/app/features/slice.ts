import type { EditorView } from "@codemirror/view";
import type { PayloadAction } from "@reduxjs/toolkit";
import { createSlice } from "@reduxjs/toolkit";
import { castDraft, WritableDraft } from "immer";

import { normalizePath } from "~/utils/normalizePath";
import type { CompilerList } from "~/hooks/compilerList";
import type { PermlinkData } from "~/hooks/permlink";
import { GithubUser } from "~/types";

export interface EditorSourceData {
  id: string;
  filename: string | null;
  text: string;
}
export type EditorViewMap = { [id: string]: EditorView | undefined };

export type EditorType = "default" | "vim";
export type TabKeyType = "2" | "4" | "8" | "tab";
export type TabWidthType = "2" | "4" | "8";
export type RunningState = "init" | "running" | "completed";

export interface EditorSettingsData {
  mode?: EditorType;
  tabKey: TabKeyType;
  tabWidth: TabWidthType;
  fixedHeight: boolean;
  fixedResultHeight?: boolean;
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

export interface HistoryEditorSourceData {
  filename: string | null;
  text: string;
}
export interface HistoryDataQuick {
  type: "quick";
  id: number;
  createdAt: number;
  currentLanguage: string;
  currentCompilerName: string;
  currentSwitches: { [name: string]: string | boolean };
  compilerOptionRaw: string;
  runtimeOptionRaw: string;
  displayName: string;
  version: string;
  sources: HistoryEditorSourceData[];
  currentTab: number;
  stdin: string;
  title: string;
  description: string;
  results: ResultData[];
}
export interface HistoryDataRun {
  type: "run";
  id: number;
  createdAt: number;
  currentLanguage: string;
  currentCompilerName: string;
  currentSwitches: { [name: string]: string | boolean };
  compilerOptionRaw: string;
  runtimeOptionRaw: string;
  displayName: string;
  version: string;
  sources: HistoryEditorSourceData[];
  currentTab: number;
  stdin: string;
  title: string;
  description: string;
  results: ResultData[];
}
export interface HistoryDataPermlink {
  type: "permlink";
  id: number;
  createdAt: number;
  permlinkId: string;
  githubUser: GithubUser | null;
  // 細かいデータはロードした時に読み込むので、
  // リスト一覧の表示に必要な情報だけ保存しておく
  currentLanguage: string;
  displayName: string;
  version: string;
  title: string;
  permlinkCreatedAt: number;
}

export type StorageExists = { [id: number]: "" };

export interface HistoryData {
  quickSaves: HistoryDataQuick[];
  histories: (HistoryDataRun | HistoryDataPermlink)[];
  keyCounter: number;
}

export type Breakpoint = "xs" | "sm" | "md" | "lg" | "xl" | "xxl";

function sourceToHistorySource(
  sources: EditorSourceData[],
  views: EditorViewMap | WritableDraft<EditorViewMap>
): HistoryEditorSourceData[] {
  return sources
    .map((s) => {
      const view = views[s.id];
      if (view === undefined && s.text === undefined) {
        return null;
      }
      return {
        filename: s.filename,
        text: view !== undefined ? view.state.doc.toString() : s.text,
      };
    })
    .filter((s) => s !== null) as HistoryEditorSourceData[];
}

export function getSourceText(
  source: EditorSourceData,
  view?: EditorView | WritableDraft<EditorView>
): string {
  return view !== undefined ? view.state.doc.toString() : source.text;
}

export function getStdin(stdin: string, stdinView?: EditorView | WritableDraft<EditorView>): string {
  return stdinView !== undefined ? stdinView!.state.doc.toString() : stdin;
}

const WANDBOX_MAX_QUICKSAVE_COUNT = 5;
const WANDBOX_MAX_HISTORY_COUNT = 50;

const initialState = {
  compilerList: null as CompilerList | null,
  permlinkData: null as PermlinkData | null,

  currentLanguage: "",
  currentCompilerName: "",
  currentSwitches: {} as { [name: string]: string | boolean },
  compilerOptionRaw: "",
  runtimeOptionRaw: "",

  title: "",
  description: "",
  author: null as GithubUser | null,
  titleDialogOpened: false,

  currentTab: 0,
  tabCounter: 0,
  sources: [
    {
      id: "wandbox-editor-main",
      filename: null,
      text: "",
    },
  ] as EditorSourceData[],
  views: {} as { [id: string]: EditorView | undefined },
  stdin: "" as string,
  stdinView: undefined as EditorView | undefined,
  stdinOpened: false,
  editorSettings: {
    mode: undefined,
    tabKey: "4",
    tabWidth: "4",
    fixedHeight: false,
    fixedResultHeight: undefined,
  } as EditorSettingsData,
  running: false,
  sharable: false,
  editorChanged: false,

  results: [] as ResultData[],

  sidebarState: "none" as "editorSettings" | "history" | "none",
  sidebarLocked: false as boolean,
  history: {
    quickSaves: [],
    histories: [],
    keyCounter: 0,
  } as HistoryData,
  storageExists: {} as StorageExists,
  tempRunData: null as HistoryDataRun | null,

  // この値が空文字以外になったらページ遷移する
  navigate: "",

  breakpoint: "md" as Breakpoint,
};

export type WandboxState = typeof initialState;

export const wandboxSlice = createSlice({
  name: "wandbox",
  initialState: initialState,
  reducers: {
    setCompilerList: (state, action: PayloadAction<CompilerList | null>) => {
      state.compilerList = action.payload;
    },
    setPermlinkData: (state, action: PayloadAction<PermlinkData | null>) => {
      state.permlinkData = action.payload;
    },
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
    setEditorSettingsMode: (state, action: PayloadAction<EditorType>) => {
      state.editorSettings.mode = action.payload;
    },
    setEditorSettingsTabKey: (state, action: PayloadAction<TabKeyType>) => {
      state.editorSettings.tabKey = action.payload;
    },
    setEditorSettingsTabWidth: (state, action: PayloadAction<TabWidthType>) => {
      state.editorSettings.tabWidth = action.payload;
    },
    setEditorSettingsFixedHeight: (state, action: PayloadAction<boolean>) => {
      state.editorSettings.fixedHeight = action.payload;
    },
    setEditorSettingsFixedResultHeight: (
      state,
      action: PayloadAction<boolean>
    ) => {
      state.editorSettings.fixedResultHeight = action.payload;
    },
    setTitle: (state, action: PayloadAction<string>) => {
      state.title = action.payload;
    },
    setDescription: (state, action: PayloadAction<string>) => {
      state.description = action.payload;
    },
    setAuthor: (state, action: PayloadAction<GithubUser | null>) => {
      state.author = action.payload;
    },
    setTitleDialogOpened: (state, action: PayloadAction<boolean>) => {
      state.titleDialogOpened = action.payload;
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
        text: text || "",
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
      state.views = {};
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
      const newFilename = normalizePath(filename).trim();
      // 既存のファイルが既にあったら変更させない
      if (
        state.sources.findIndex((source) => source.filename === newFilename) !==
        -1
      ) {
        return;
      }
      // 空文字
      if (newFilename.length === 0) {
        return;
      }

      state.sources[tab].filename = newFilename;
    },
    setView: (
      state,
      action: PayloadAction<{ id: string; view: EditorView | undefined }>
    ) => {
      const { id, view } = action.payload;
      state.views[id] = castDraft(view);
    },
    setStdin: (state, action: PayloadAction<string>) => {
      state.stdin = action.payload;
    },
    setStdinView: (state, action: PayloadAction<EditorView | undefined>) => {
      state.stdinView = castDraft(action.payload);
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
    setEditorChanged: (state, action: PayloadAction<boolean>) => {
      state.editorChanged = action.payload;
    },
    pushQuickSave: (state) => {
      if (state.compilerList === null) {
        return;
      }
      const ci = state.compilerList.compilers.find(
        (x) => x.name === state.currentCompilerName
      );
      if (ci === undefined) {
        return;
      }

      const historyData: HistoryDataQuick = {
        type: "quick",
        id: state.history.keyCounter,
        createdAt: Math.floor(Date.now() / 1000),
        currentLanguage: state.currentLanguage,
        currentCompilerName: state.currentCompilerName,
        currentSwitches: state.currentSwitches,
        compilerOptionRaw: state.compilerOptionRaw,
        runtimeOptionRaw: state.runtimeOptionRaw,
        displayName: ci.displayName,
        version: ci.version,
        sources: sourceToHistorySource(state.sources, state.views),
        currentTab: state.currentTab,
        stdin: getStdin(state.stdin, state.stdinView),
        title: state.title,
        description: state.description,
        results: state.results,
      };
      const h = state.history;
      h.quickSaves.push(historyData);
      if (h.quickSaves.length > WANDBOX_MAX_QUICKSAVE_COUNT) {
        h.quickSaves.shift();
      }
      h.keyCounter += 1;
    },
    prepareRun: (state) => {
      if (state.compilerList === null) {
        return;
      }
      const ci = state.compilerList.compilers.find(
        (x) => x.name === state.currentCompilerName
      );
      if (ci === undefined) {
        return;
      }

      const historyData: HistoryDataRun = {
        type: "run",
        id: state.history.keyCounter,
        createdAt: Math.floor(Date.now() / 1000),
        currentLanguage: state.currentLanguage,
        currentCompilerName: state.currentCompilerName,
        currentSwitches: state.currentSwitches,
        compilerOptionRaw: state.compilerOptionRaw,
        runtimeOptionRaw: state.runtimeOptionRaw,
        displayName: ci.displayName,
        version: ci.version,
        sources: sourceToHistorySource(state.sources, state.views),
        currentTab: state.currentTab,
        stdin: getStdin(state.stdin, state.stdinView),
        title: state.title,
        description: state.description,
        // results は最終的に結果が得られた後に設定する
        results: [],
      };

      state.tempRunData = historyData;
    },
    commitRun: (state) => {
      if (state.tempRunData === null) {
        return;
      }

      const historyData = {
        ...state.tempRunData,
        results: state.results,
      };
      const h = state.history;
      h.histories.push(historyData);
      if (h.histories.length > WANDBOX_MAX_HISTORY_COUNT) {
        h.histories.shift();
      }
      h.keyCounter += 1;
    },
    pushPermlink: (
      state,
      action: PayloadAction<{
        permlinkId: string;
        githubUser: GithubUser | null;
        currentLanguage: string;
        displayName: string;
        version: string;
        title: string;
        permlinkCreatedAt: number;
      }>
    ) => {
      const {
        permlinkId,
        githubUser,
        currentLanguage,
        displayName,
        version,
        title,
        permlinkCreatedAt,
      } = action.payload;

      const historyData: HistoryDataPermlink = {
        type: "permlink",
        id: state.history.keyCounter,
        createdAt: Math.floor(Date.now() / 1000),
        permlinkId,
        githubUser,
        currentLanguage,
        displayName,
        version,
        title,
        permlinkCreatedAt,
      };

      const h = state.history;

      // 同じ permlink は削除する
      h.histories = h.histories.filter(
        (x) => !(x.type === "permlink" && x.permlinkId === permlinkId)
      );

      h.histories.push(historyData);

      // 最大数を超えたら古いのを削除する
      if (h.histories.length > WANDBOX_MAX_HISTORY_COUNT) {
        h.histories.shift();
      }
      h.keyCounter += 1;
    },
    setSidebarState: (
      state,
      action: PayloadAction<WandboxState["sidebarState"]>
    ) => {
      state.sidebarState = action.payload;
    },
    setSidebarLocked: (state, action: PayloadAction<boolean>) => {
      state.sidebarLocked = action.payload;
    },
    initHistory: (
      state,
      action: PayloadAction<{
        history: HistoryData;
        storageExists: StorageExists;
      }>
    ) => {
      const { history, storageExists } = action.payload;
      state.history = history;
      state.storageExists = storageExists;
    },
    setStorageExists: (state, action: PayloadAction<StorageExists>) => {
      state.storageExists = action.payload;
    },

    setNavigate: (state, action: PayloadAction<string>) => {
      state.navigate = action.payload;
    },
    clearNavigate: (state) => {
      state.navigate = "";
    },

    loadQuickSave: (
      state,
      action: PayloadAction<HistoryDataQuick | HistoryDataRun>
    ) => {
      const x = action.payload;
      state.currentLanguage = x.currentLanguage;
      state.currentCompilerName = x.currentCompilerName;
      state.currentSwitches = x.currentSwitches;
      state.compilerOptionRaw = x.compilerOptionRaw;
      state.runtimeOptionRaw = x.runtimeOptionRaw;
      state.currentTab = x.currentTab;
      state.stdin = x.stdin;
      if (x.stdin.length !== 0) {
        state.stdinOpened = true;
      }
      state.title = x.title;
      state.description = x.description;
      state.results = x.results;

      let counter = 0;
      state.sources = [];
      for (const source of x.sources) {
        const id =
          source.filename === null
            ? "wb-editor-main"
            : `wb-editor-tab${counter}`;
        state.sources.push({
          id: id,
          filename: source.filename,
          text: source.text,
        });
        counter += 1;
      }
      state.tabCounter = counter;

      if (x.type === "run") {
        state.sharable = true;
      }
    },

    setBreakpoint: (state, action: PayloadAction<Breakpoint>) => {
      state.breakpoint = action.payload;
    },
  },
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  extraReducers: (builder) => { },
});

export const wandboxReducer = wandboxSlice.reducer;
export const wandboxActions = wandboxSlice.actions;
export const wandboxInitialState = initialState;
