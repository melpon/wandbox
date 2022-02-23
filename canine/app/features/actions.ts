import type { EditorView } from "@codemirror/view";

import type { AppDispatch } from "~/store";
import type {
  EditorSettingsData,
  EditorSourceData,
  HistoryData,
  StorageExists,
} from "./slice";
import {
  wandboxInitialState,
  wandboxSlice,
  WandboxState,
  getSourceText,
  getStdin,
} from "./slice";

const actions = wandboxSlice.actions;

export { getSourceText, getStdin };

export function addSource(
  dispatch: AppDispatch,
  sources: EditorSourceData[],
  filename: string,
  text?: string
): number {
  dispatch(actions.addSource({ filename, text }));
  return sources.length;
}

export function setView(
  dispatch: AppDispatch,
  sources: EditorSourceData[],
  tab: number,
  view: EditorView
) {
  if (sources[tab].text !== undefined) {
    view.dispatch({
      changes: {
        from: 0,
        to: view.state.doc.length,
        insert: sources[tab].text,
      },
    });
  }

  dispatch(actions.setView({ id: sources[tab].id, view }));
}

const WANDBOX_QUICKSAVES_KEY = "wandbox.quicksaves";
const WANDBOX_HISTORIES_KEY = "wandbox.histories";
const WANDBOX_KEYCOUNTER_KEY = "wandbox.keycounter";
const WANDBOX_DATA_KEY_PREFIX = "wd.";

// 履歴データを Local Storage に保存する
// 既にどのデータが保存されているかどうかは storageExists に入っているので、
// それを使ってうまいこと差分だけ保存する。
export function saveHistory(
  history: HistoryData,
  storageExists: StorageExists
): StorageExists {
  const qsEntries = history.quickSaves.map((x) => [x.id, ""]);
  const histEntries = history.histories.map((x) => [x.id, ""]);
  const usedIds: StorageExists = Object.fromEntries(
    qsEntries.concat(histEntries)
  );
  const allIds = Object.assign({ ...usedIds }, storageExists);
  const newIds: StorageExists = {};
  const removeIds: StorageExists = {};
  for (const id in allIds) {
    // 現在使われているけど Local Storage に保存されていない場合は保存する
    if (id in usedIds && !(id in storageExists)) {
      newIds[id] = "";
    }
    // Local Storage に保存されてるけど現在使われていない場合は削除する
    if (id in storageExists && !(id in usedIds)) {
      removeIds[id] = "";
    }
  }

  // 削除
  for (const id in removeIds) {
    const key = `${WANDBOX_DATA_KEY_PREFIX}${id}`;
    localStorage.removeItem(key);
  }

  // 保存
  for (const x of history.quickSaves) {
    if (!(x.id in newIds)) {
      continue;
    }
    const key = `${WANDBOX_DATA_KEY_PREFIX}${x.id}`;
    localStorage.setItem(key, JSON.stringify(x));
  }
  for (const x of history.histories) {
    if (!(x.id in newIds)) {
      continue;
    }
    const key = `${WANDBOX_DATA_KEY_PREFIX}${x.id}`;
    localStorage.setItem(key, JSON.stringify(x));
  }

  // 必須項目の保存
  localStorage.setItem(
    WANDBOX_QUICKSAVES_KEY,
    JSON.stringify(history.quickSaves.map((x) => x.id))
  );
  localStorage.setItem(
    WANDBOX_HISTORIES_KEY,
    JSON.stringify(history.histories.map((x) => x.id))
  );
  localStorage.setItem(
    WANDBOX_KEYCOUNTER_KEY,
    JSON.stringify(history.keyCounter)
  );

  return usedIds;
}

// Local Storage から履歴データを読み込む
export function loadHistory(): [HistoryData, StorageExists] {
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
    const key = `${WANDBOX_DATA_KEY_PREFIX}${id}`;
    const v = localStorage.getItem(key);
    if (v === null) {
      continue;
    }
    storageExists[id] = "";
    quickSaves.push(JSON.parse(v));
  }
  for (const id of historyIds) {
    const key = `${WANDBOX_DATA_KEY_PREFIX}${id}`;
    const v = localStorage.getItem(key);
    if (v === null) {
      continue;
    }
    storageExists[id] = "";
    histories.push(JSON.parse(v));
  }

  return [{ keyCounter, quickSaves, histories }, storageExists];
}

const WANDBOX_SETTINGS_KEY = "wandbox.settings";

interface PersistentSettingsData {
  editorSettings: EditorSettingsData;
  sidebarState: WandboxState["sidebarState"];
  sidebarLocked: boolean;
}

export function saveSettings({
  editorSettings,
  sidebarState,
  sidebarLocked,
}: PersistentSettingsData) {
  const item = {
    editorSettings,
    sidebarState,
    sidebarLocked,
  };
  localStorage.setItem(WANDBOX_SETTINGS_KEY, JSON.stringify(item));
}

export function loadSettings(): PersistentSettingsData {
  const item = localStorage.getItem(WANDBOX_SETTINGS_KEY);
  if (item === null) {
    const { editorSettings, sidebarLocked, sidebarState } = wandboxInitialState;
    return {
      editorSettings,
      sidebarLocked,
      sidebarState,
    };
  }
  return JSON.parse(item);
}

export function applySettings(
  dispatch: AppDispatch,
  settings: PersistentSettingsData
) {
  dispatch(actions.setEditorSettingsTabKey(settings.editorSettings.tabKey));
  dispatch(actions.setEditorSettingsTabWidth(settings.editorSettings.tabWidth));
  dispatch(
    actions.setEditorSettingsFixedHeight(settings.editorSettings.fixedHeight)
  );
  dispatch(actions.setSidebarLocked(settings.sidebarLocked));
  dispatch(actions.setSidebarState(settings.sidebarState));
}
