import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { useLoaderData, useParams } from "@remix-run/react";

import { CompilerList } from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { Header } from "~/components/Header";
import { Compiler } from "~/components/Compiler";
import { Permlink } from "~/components/Permlink";
import { Editor } from "~/components/Editor";
import { Command } from "~/components/Command";
import { Result } from "~/components/Result";
import { Run } from "~/components/Run";
import { Title, TitleDialog } from "~/components/Title";
import { Author, AuthorData } from "~/components/Author";
import Sidebar from "~/components/react-sidebar/Sidebar";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import {
  applySettings,
  loadHistory,
  loadSettings,
  saveHistory,
  saveSettings,
} from "~/features/actions";
import { SidebarBase } from "~/components/SidebarBase";
import i18n from "~/i18n";
import { SponsorsGetData } from "~/hooks/sponsors";
import { Sponsors } from "./Sponsors";
import { UpdateBreakpoint } from "./UpdateBreakpoint";
import { WandboxLoaderData } from "~/types";

const Wandbox: React.FC = (): React.ReactElement | null => {
  // 参照しておかないとグローバルな初期化コード自体が消えてしまうので、
  // 適当にアクセスするコードを書いておく
  i18n;

  const { permlinkId } = useParams();
  const {
    compilerList,
    sponsors,
    permlinkData
  }: {
    compilerList: CompilerList,
    sponsors: SponsorsGetData,
    permlinkData: PermlinkData | null
  } = useLoaderData<WandboxLoaderData>();

  const [localStorageChanged, setLocalStorageChanged] = useState(false);

  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  useEffect((): void => {
    if (permlinkData === null) {
      return;
    }

    // stdin がある場合は標準入力用のエディタを開く
    if (permlinkData.parameter.stdin.length !== 0) {
      dispatch(actions.setStdinOpened(true));
    }
  }, []);

  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    currentTab,
    title,
    description,
    author,
    editorChanged,
    results,
    editorSettings,
    sidebarState,
    sidebarLocked,
    history,
    storageExists,
  } = useSelector(
    ({
      wandbox: {
        currentLanguage,
        currentCompilerName,
        currentSwitches,
        compilerOptionRaw,
        runtimeOptionRaw,
        currentTab,
        title,
        description,
        author,
        editorChanged,
        results,
        editorSettings,
        sidebarState,
        sidebarLocked,
        history,
        storageExists,
      },
    }: AppState) => ({
      currentLanguage,
      currentCompilerName,
      currentSwitches,
      compilerOptionRaw,
      runtimeOptionRaw,
      currentTab,
      title,
      description,
      author,
      editorChanged,
      results,
      editorSettings,
      sidebarState,
      sidebarLocked,
      history,
      storageExists,
    })
  );

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

  // 編集中のデータが変化したら保存する
  useEffect(() => {
    dispatch(actions.setEditorChanged(true));
  }, [
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    currentTab,
    title,
    description,
    results,
  ]);
  useEffect(() => {
    // 共有画面では保存しない
    if (permlinkId !== undefined) {
      return;
    }
    if (!editorChanged) {
      return;
    }

    const timer = setTimeout(() => {
      dispatch(actions.pushQuickSave(compilerList));
      dispatch(actions.setEditorChanged(false));
    }, 10000);
    return () => {
      clearTimeout(timer);
    };
  }, [editorChanged]);

  // 適切にタイトルを書き換える
  useEffect(() => {
    if (typeof document === "undefined") {
      return;
    }
    let language: string;
    let docTitle: string;
    if (permlinkData === null) {
      language = currentLanguage;
      if (title.length !== 0) {
        docTitle = title;
      } else if (currentCompilerName.length !== 0) {
        const ci = compilerList.compilers.find(
          (x) => x.name === currentCompilerName
        );
        if (ci === undefined) {
          docTitle = "";
        } else {
          docTitle = `${ci.displayName} ${ci.version}`;
        }
      } else {
        docTitle = "";
      }
    } else {
      language = permlinkData.parameter.compilerInfo.language;
      if (permlinkData.parameter.title.length !== 0) {
        docTitle = permlinkData.parameter.title;
      } else {
        docTitle = `${permlinkData.parameter.compilerInfo.displayName} ${permlinkData.parameter.compilerInfo.version}`;
      }
    }
    document.title =
      (language.length === 0 ? "" : `[${language}] `) +
      (docTitle.length === 0 ? "" : `${docTitle} - `) +
      "Wandbox";
  }, [currentLanguage, title, currentCompilerName]);

  const sidebarContent = <SidebarBase compilerList={compilerList} />;

  return (
    <div id="wb-main" className="d-flex flex-column">
      <UpdateBreakpoint />
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
        contentClassName={`${sidebarLocked ? "wb-sidebar-locked" : ""} py-24px px-8px px-md-32px d-flex flex-column`}
        contentId="wb-main-content"
      >
        <div className="d-flex flex-column flex-md-row gap-16px">
          <TitleDialog />
          {permlinkData !== null && <AuthorData permlinkData={permlinkData} />}

          <div className="d-flex d-md-none flex-column gap-16px">
            <Title permlinkData={permlinkData} />
            {permlinkData !== null && (
              <div className="align-self-end">
                <Author permlinkData={permlinkData} author={author} />
              </div>
            )}
          </div>

          <div className="d-flex flex-column">
            <Compiler compilerList={compilerList} permlinkData={permlinkData} />
            {(sponsors.corporate.length !== 0 ||
              sponsors.personal.length !== 0) && (
                <>
                  <div className="wb-line-horizontal my-16px d-none d-md-block" />
                  <div className="d-none d-md-block">
                    <Sponsors sponsors={sponsors} />
                  </div>
                </>
              )}
          </div>
          <div className="flex-grow-1 d-flex flex-column gap-8px">
            <div className="d-none d-md-flex gap-16px">
              <Title permlinkData={permlinkData} />
              {permlinkData !== null && (
                <Author permlinkData={permlinkData} author={author} />
              )}
            </div>
            <Editor compilerList={compilerList} permlinkData={permlinkData} />
            {(currentCompilerName !== "" || permlinkData !== null) && (
              <>
                <Command
                  compilerList={compilerList}
                  permlinkData={permlinkData}
                />
                <div className="d-flex gap-8px">
                  <Run
                    compilerList={compilerList}
                    permlinkData={permlinkData}
                  />
                  <Permlink
                    compilerList={compilerList}
                    permlinkData={permlinkData}
                  />
                </div>
              </>
            )}
            <Result permlinkData={permlinkData} />
          </div>
        </div>
        {sponsors !== null &&
          (sponsors.corporate.length !== 0 ||
            sponsors.personal.length !== 0) && (
            <>
              <div className="wb-line-horizontal my-16px d-block d-md-none" />
              <div className="d-block d-md-none">
                <Sponsors sponsors={sponsors} />
              </div>
            </>
          )}
        {/* この div を入れておくことで padding-bottom が効くようになる */}
        <div></div>
      </Sidebar>
    </div>
  );
};

export { Wandbox };
