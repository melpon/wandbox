import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { Header } from "~/components/Header";
import { Compiler } from "~/components/Compiler";
import { Permlink } from "~/components/Permlink";
import { Editor } from "~/components/Editor";
import { Command } from "~/components/Command";
import { Result } from "~/components/Result";
import { Run } from "~/components/Run";
import { Title } from "~/components/Title";
import { useParams } from "remix";
import { Author } from "~/components/Author";
import Sidebar from "~/components/react-sidebar/Sidebar";
import { AppState, useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import {
  applySettings,
  loadHistory,
  loadSettings,
  saveHistory,
  saveSettings,
} from "~/features/actions";
import { SidebarBase } from "~/components/SidebarBase";

const Wandbox: React.FC = (): React.ReactElement | null => {
  const { permlinkId } = useParams();
  const [, setError] = useError();
  const compilerList = useCompilerList(
    `${WANDBOX_URL_PREFIX}/api/list.json`,
    setError
  );

  const [permlinkData, setPermlinkData] = useState<PermlinkData | null>(null);
  const [permlinkResp, , doGetPermlink] = useGetPermlink(
    permlinkId === undefined ? "" : permlinkId,
    setError
  );
  const [localStorageChanged, setLocalStorageChanged] = useState(false);

  useEffect((): void => {
    if (permlinkId === undefined) {
      setPermlinkData(null);
      return;
    }

    doGetPermlink(`${WANDBOX_URL_PREFIX}/api/permlink/${permlinkId}`, {});
  }, [permlinkId]);

  useEffect((): void => {
    if (permlinkResp === null) {
      return;
    }

    setPermlinkData(permlinkResp);
    // stdin がある場合は標準入力用のエディタを開く
    if (permlinkResp.parameter.stdin.length !== 0) {
      dispatch(actions.setStdinOpened(true));
    }
  }, [permlinkResp]);

  const {
    currentCompilerName,
    editorSettings,
    sidebarState,
    sidebarLocked,
    history,
    storageExists,
  } = useSelector(
    ({
      wandbox: {
        currentCompilerName,
        editorSettings,
        sidebarState,
        sidebarLocked,
        history,
        storageExists,
      },
    }: AppState) => ({
      currentCompilerName,
      editorSettings,
      sidebarState,
      sidebarLocked,
      history,
      storageExists,
    })
  );
  const dispatch = useAppDispatch();

  const actions = wandboxSlice.actions;
  // 設定データのロード（初回に一回だけ読み込む）
  useEffect((): void => {
    const settings = loadSettings();
    applySettings(dispatch, settings);
  }, []);

  // 設定に変更があったら即座に保存する
  useEffect((): void => {
    saveSettings({ editorSettings, sidebarState, sidebarLocked });
  }, [editorSettings, sidebarState, sidebarLocked]);

  // 履歴情報のロード
  useEffect(() => {
    const [history, storageExists] = loadHistory();
    dispatch(actions.initHistory({ history, storageExists }));
    // permlink 経由でない場合、クイックセーブした情報を読み込む
    if (permlinkId === undefined) {
      const qss = history.quickSaves;
      if (qss.length !== 0) {
        dispatch(actions.loadQuickSave(qss[qss.length - 1]));
      }
    }
  }, []);

  // 履歴の差分が出たら Local Storage に保存する
  useEffect(() => {
    const newStorageExists = saveHistory(history, storageExists);
    dispatch(actions.setStorageExists(newStorageExists));
  }, [history]);

  // 他のタブで localStorage が更新されたら、
  // アクティブになった際に情報を読み込み直す
  useEffect(() => {
    const listener = () => {
      setLocalStorageChanged(true);
    };

    addEventListener("storage", listener);
    return () => {
      removeEventListener("storage", listener);
    };
  }, []);
  useEffect(() => {
    const listener = () => {
      if (localStorageChanged) {
        const [history, storageExists] = loadHistory();
        dispatch(actions.initHistory({ history, storageExists }));
        setLocalStorageChanged(false);
      }
    };

    addEventListener("focus", listener);
    return () => {
      removeEventListener("focus", listener);
    };
  }, [localStorageChanged]);

  if (compilerList === null) {
    return null;
  }

  const sidebarContent = <SidebarBase />;

  return (
    <div id="wb-main" className="d-flex flex-column">
      <Header />
      <Sidebar
        open={sidebarState !== "none"}
        docked={sidebarLocked}
        sidebar={sidebarContent}
        onSetOpen={(open) => {
          if (open) {
            dispatch(actions.setSidebarState("history"));
          } else {
            dispatch(actions.setSidebarState("none"));
            dispatch(actions.setSidebarLocked(false));
          }
        }}
        pullRight={true}
        rootClassName="wb-sidebar-root"
        sidebarClassName="wb-sidebar"
        contentClassName="py-24px px-32px d-flex gap-16px"
        styles={{
          sidebar: {
            boxShadow: "-3px 2px 4px rgba(36, 41, 47, 0.15)",
          },
        }}
      >
        <Compiler compilerList={compilerList} permlinkData={permlinkData} />
        <div className="flex-grow-1 d-flex flex-column gap-8px">
          <div className="d-flex gap-16px">
            <Title className="flex-grow-1" permlinkData={permlinkData} />
            {permlinkData !== null && <Author permlinkData={permlinkData} />}
          </div>
          <Editor compilerList={compilerList} permlinkData={permlinkData} />
          {(currentCompilerName !== "" || permlinkData !== null) && (
            <>
              <Command
                compilerList={compilerList}
                permlinkData={permlinkData}
              />
              <div className="d-flex gap-8px">
                <Run compilerList={compilerList} permlinkData={permlinkData} />
                <Permlink
                  compilerList={compilerList}
                  permlinkData={permlinkData}
                  clearPermlinkData={() => {}}
                />
              </div>
            </>
          )}
          <Result permlinkData={permlinkData} />
        </div>
      </Sidebar>
    </div>
  );
};

export { Wandbox };
