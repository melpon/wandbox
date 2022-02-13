import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
} from "react";
import { ResultData, WandboxState } from "~/features/slice";

export interface EditorSourceData {
  filename: string | null;
  text: string;
}
export interface CompilerData {
  currentLanguage: string;
  currentCompilerName: string;
  currentSwitches: { [name: string]: string | boolean };
  compilerOptionRaw: string;
  runtimeOptionRaw: string;
}
export interface EditorData {
  sources: EditorSourceData[];
  currentTab: number;
  stdin: string;
  title: string;
  description: string;
}
export interface HistoryDataQuick {
  type: "quick";
  id: number;
  createdAt: number;
  compiler: CompilerData;
  editor: EditorData;
  results: ResultData[];
}
export interface HistoryDataRun {
  type: "run";
  id: number;
  createdAt: number;
  compiler: CompilerData;
  editor: EditorData;
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
  currentCompilerName: string;
  title: string;
  permlinkCreatedAt: number;
}

type StorageExists = { [id: number]: "" };

export interface HistoryData {
  quickSaves: HistoryDataQuick[];
  histories: (HistoryDataRun | HistoryDataPermlink)[];
  keyCounter: number;
}

const WANDBOX_QUICKSAVES_KEY = "wandbox.quicksaves";
const WANDBOX_HISTORIES_KEY = "wandbox.histories";
const WANDBOX_KEYCOUNTER_KEY = "wandbox.keycounter";
const WANDBOX_QUICKSAVE_KEY_PREFIX = "wq.";
const WANDBOX_HISTORY_KEY_PREFIX = "wh.";

const WANDBOX_MAX_QUICKSAVE_COUNT = 5;
const WANDBOX_MAX_HISTORY_COUNT = 50;

export interface History {
  data: HistoryData;
  pushQuickSave: (state: WandboxState) => void;
  prepareRun: (state: WandboxState) => void;
  commitRun: (state: WandboxState) => void;
  pushPermlink: (
    permlinkId: string,
    githubUser: GithubUser | null,
    currentLanguage: string,
    currentCompilerName: string,
    title: string,
    permlinkCreatedAt: number
  ) => void;
}

function saveHistory(
  data: HistoryData,
  storageExists: StorageExists
): StorageExists {
  const newStorageExists = { ...storageExists };

  console.log("quickSaves", data.quickSaves);
  console.log("storageExists", storageExists);

  // 未セーブのデータを設定していく
  for (const x of data.quickSaves) {
    if (x.id in newStorageExists) {
      continue;
    }
    const key = `${WANDBOX_QUICKSAVE_KEY_PREFIX}${x.id}`;
    localStorage.setItem(key, JSON.stringify(x));
    newStorageExists[x.id] = "";
  }
  for (const x of data.histories) {
    if (x.id in newStorageExists) {
      continue;
    }
    const key = `${WANDBOX_HISTORY_KEY_PREFIX}${x.id}`;
    localStorage.setItem(key, JSON.stringify(x));
    newStorageExists[x.id] = "";
  }

  localStorage.setItem(
    WANDBOX_QUICKSAVES_KEY,
    JSON.stringify(data.quickSaves.map((x) => x.id))
  );
  localStorage.setItem(
    WANDBOX_HISTORIES_KEY,
    JSON.stringify(data.histories.map((x) => x.id))
  );
  localStorage.setItem(WANDBOX_KEYCOUNTER_KEY, JSON.stringify(data.keyCounter));

  return newStorageExists;
}

function loadHistory(): [HistoryData, StorageExists] {
  const keyCounter: HistoryData["keyCounter"] = JSON.parse(
    localStorage.getItem(WANDBOX_KEYCOUNTER_KEY) || "0"
  );
  const quickSaveIds: number[] = JSON.parse(
    localStorage.getItem(WANDBOX_QUICKSAVES_KEY) || "[]"
  );
  const historyIds: number[] = JSON.parse(
    localStorage.getItem(WANDBOX_HISTORIES_KEY) || "[]"
  );
  const quickSaves: HistoryData["quickSaves"] = [];
  const histories: HistoryData["histories"] = [];
  const storageExists: StorageExists = {};
  for (const id of quickSaveIds) {
    const key = `${WANDBOX_QUICKSAVE_KEY_PREFIX}${id}`;
    const v = localStorage.getItem(key);
    if (v === null) {
      continue;
    }
    storageExists[id] = "";
    quickSaves.push(JSON.parse(v));
  }
  for (const id of historyIds) {
    const key = `${WANDBOX_HISTORY_KEY_PREFIX}${id}`;
    const v = localStorage.getItem(key);
    if (v === null) {
      continue;
    }
    storageExists[id] = "";
    histories.push(JSON.parse(v));
  }

  return [{ keyCounter, quickSaves, histories }, storageExists];
}

function createHistoryDataQuick(
  id: number,
  state: WandboxState
): HistoryDataQuick {
  const sources = state.sources
    .map((s) => {
      if (s.view === undefined && s.text === undefined) {
        return null;
      }
      return {
        filename: s.filename,
        text: s.text !== undefined ? s.text : s.view!.state.doc.toString(),
      };
    })
    .filter((s) => s !== null) as EditorSourceData[];
  return {
    type: "quick",
    id,
    createdAt: Math.floor(Date.now() / 1000),
    compiler: {
      currentLanguage: state.currentLanguage,
      currentCompilerName: state.currentCompilerName,
      currentSwitches: state.currentSwitches,
      compilerOptionRaw: state.compilerOptionRaw,
      runtimeOptionRaw: state.runtimeOptionRaw,
    },
    editor: {
      sources: sources,
      currentTab: state.currentTab,
      stdin: state.stdin,
      title: state.title,
      description: state.description,
    },
    results: state.results,
  };
}

function createHistoryDataRun(id: number, state: WandboxState): HistoryDataRun {
  const sources = state.sources
    .map((s) => {
      if (s.view === undefined && s.text === undefined) {
        return null;
      }
      return {
        filename: s.filename,
        text: s.text !== undefined ? s.text : s.view!.state.doc.toString(),
      };
    })
    .filter((s) => s !== null) as EditorSourceData[];
  return {
    type: "run",
    id,
    createdAt: Math.floor(Date.now() / 1000),
    compiler: {
      currentLanguage: state.currentLanguage,
      currentCompilerName: state.currentCompilerName,
      currentSwitches: state.currentSwitches,
      compilerOptionRaw: state.compilerOptionRaw,
      runtimeOptionRaw: state.runtimeOptionRaw,
    },
    editor: {
      sources: sources,
      currentTab: state.currentTab,
      stdin: state.stdin,
      title: state.title,
      description: state.description,
    },
    // results は最終的に結果が得られた後に設定する
    results: [],
  };
}

function createHistoryDataPermlink(
  id: number,
  permlinkId: string,
  githubUser: GithubUser | null,
  currentLanguage: string,
  currentCompilerName: string,
  title: string,
  permlinkCreatedAt: number
): HistoryDataPermlink {
  return {
    type: "permlink",
    id,
    createdAt: Math.floor(Date.now() / 1000),
    permlinkId,
    githubUser,
    currentLanguage,
    currentCompilerName,
    title,
    permlinkCreatedAt,
  };
}

function removeQuickSave(
  id: number,
  quickSaves: HistoryData["quickSaves"],
  storageExists: StorageExists
): [HistoryData["quickSaves"], StorageExists] {
  if (!(id in storageExists)) {
    return [quickSaves, storageExists];
  }
  const newQuickSaves = quickSaves.filter((x) => x.id !== id);
  const newStorageExists = { ...storageExists };
  delete newStorageExists[id];
  const key = `${WANDBOX_QUICKSAVE_KEY_PREFIX}${id}`;
  localStorage.removeItem(key);
  return [newQuickSaves, newStorageExists];
}

function removeHistory(
  id: number,
  histories: HistoryData["histories"],
  storageExists: StorageExists
): [HistoryData["histories"], StorageExists] {
  if (!(id in storageExists)) {
    return [histories, storageExists];
  }
  const newHistories = histories.filter((x) => x.id !== id);
  const newStorageExists = { ...storageExists };
  delete newStorageExists[id];
  const key = `${WANDBOX_HISTORY_KEY_PREFIX}${id}`;
  localStorage.removeItem(key);
  return [newHistories, newStorageExists];
}

function useHistory(): History {
  const [data, setData] = useState<HistoryData>({
    quickSaves: [],
    histories: [],
    keyCounter: 0,
  });
  const [storageExists, setStorageExists] = useState<StorageExists>({});
  const [tempRunData, setTempRunData] = useState<HistoryDataRun | null>(null);

  useEffect(() => {
    const [data, storageExists] = loadHistory();
    setData(data);
    setStorageExists(storageExists);
  }, []);

  const pushQuickSave = useCallback(
    (state: WandboxState) => {
      const historyData = createHistoryDataQuick(data.keyCounter, state);
      let newQuickSaves = [...data.quickSaves, historyData];
      let newStorageExists = storageExists;
      if (newQuickSaves.length > WANDBOX_MAX_QUICKSAVE_COUNT) {
        const v = newQuickSaves.pop()!;
        [newQuickSaves, newStorageExists] = removeQuickSave(
          v.id,
          newQuickSaves,
          newStorageExists
        );
      }
      const newData = {
        ...data,
        quickSaves: newQuickSaves,
        keyCounter: data.keyCounter + 1,
      };
      newStorageExists = saveHistory(newData, newStorageExists);
      setData(newData);
      setStorageExists(newStorageExists);
    },
    [data, storageExists]
  );

  const prepareRun = useCallback(
    (state: WandboxState) => {
      const historyData = createHistoryDataRun(data.keyCounter, state);
      setTempRunData(historyData);
      console.log("data", data);
      console.log("historyData", historyData);
    },
    [data]
  );

  const commitRun = useCallback(
    (state: WandboxState) => {
      console.log("tempRunData", tempRunData);
      if (tempRunData === null) {
        return;
      }

      const historyData = {
        ...tempRunData,
        results: state.results,
      };
      let newHistories = [...data.histories, historyData];
      let newStorageExists = storageExists;
      if (newHistories.length > WANDBOX_MAX_HISTORY_COUNT) {
        const v = newHistories.pop()!;
        [newHistories, newStorageExists] = removeHistory(
          v.id,
          newHistories,
          newStorageExists
        );
      }
      const newData = {
        ...data,
        histories: newHistories,
        keyCounter: data.keyCounter + 1,
      };
      newStorageExists = saveHistory(newData, newStorageExists);
      setData(newData);
      setStorageExists(newStorageExists);
      setTempRunData(null);
    },
    [tempRunData, data, storageExists]
  );

  const pushPermlink = useCallback(
    (
      permlinkId: string,
      githubUser: GithubUser | null,
      currentLanguage: string,
      currentCompilerName: string,
      title: string,
      permlinkCreatedAt: number
    ) => {
      const historyData = createHistoryDataPermlink(
        data.keyCounter,
        permlinkId,
        githubUser,
        currentLanguage,
        currentCompilerName,
        title,
        permlinkCreatedAt
      );
      // 同じ permlink は削除する
      let newHistories = data.histories;
      let newStorageExists = storageExists;
      for (let i = 0; i < newHistories.length; ) {
        const v = newHistories[i];
        if (v.type !== "permlink") {
          i++;
          continue;
        }
        if (permlinkId !== v.permlinkId) {
          i++;
          continue;
        }
        [newHistories, newStorageExists] = removeHistory(
          v.id,
          newHistories,
          newStorageExists
        );
      }

      newHistories = [...newHistories, historyData];

      // 最大数を超えたら古いのを削除する
      if (newHistories.length > WANDBOX_MAX_HISTORY_COUNT) {
        const v = newHistories[0];
        [newHistories, newStorageExists] = removeHistory(
          v.id,
          newHistories,
          newStorageExists
        );
      }
      const newData = {
        ...data,
        histories: newHistories,
        keyCounter: data.keyCounter + 1,
      };
      newStorageExists = saveHistory(newData, newStorageExists);
      setData(newData);
      setStorageExists(newStorageExists);
    },
    [data, storageExists]
  );

  return useMemo(
    () => ({
      data,
      pushQuickSave,
      prepareRun,
      commitRun,
      pushPermlink,
    }),
    [data, pushQuickSave, prepareRun, commitRun, pushPermlink]
  );
}

export interface SidebarContextState {
  opened: boolean;
  locked: boolean;
  history: History;

  setOpened: (opened: boolean) => void;
  setLocked: (locked: boolean) => void;
}

function useSidebarContextState(): SidebarContextState {
  const [opened, setOpened] = useState(false);
  const [locked, setLocked] = useState(false);
  const history = useHistory();

  return useMemo(
    () => ({
      opened,
      locked,
      history,
      setOpened,
      setLocked,
    }),
    [opened, locked, history, setOpened, setLocked]
  );
}

const SidebarContext = createContext<SidebarContextState | null>(null);

function useSidebarContext(): SidebarContextState {
  return useContext(SidebarContext) as SidebarContextState;
}

export { SidebarContext, useSidebarContextState, useSidebarContext };
