import React, { useCallback, useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { useParams } from "remix";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink } from "~/hooks/permlink";
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
import type { Breakpoint } from "~/features/slice";
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
import { useGetSponsors } from "~/hooks/sponsors";
import { Sponsors } from "./Sponsors";

const Wandbox: React.FC = (): React.ReactElement | null => {
  // 参照しておかないとグローバルな初期化コード自体が消えてしまうので、
  // 適当にアクセスするコードを書いておく
  i18n;

  const { permlinkId } = useParams();
  const [, setError] = useError();
  const compilerList = useCompilerList(`/api/list.json`, setError);

  const [permlinkResp, , doGetPermlink] = useGetPermlink(
    permlinkId === undefined ? "" : permlinkId,
    setError
  );
  const [localStorageChanged, setLocalStorageChanged] = useState(false);

  const [sponsors, , doGetSponsors] = useGetSponsors(
    "/api/sponsors.json",
    setError
  );

  useEffect((): void => {
    if (permlinkId === undefined) {
      dispatch(actions.setPermlinkData(null));
      return;
    }

    doGetPermlink(`/api/permlink/${permlinkId}`, {});
  }, [permlinkId]);

  useEffect((): void => {
    if (permlinkResp === null) {
      return;
    }

    dispatch(actions.setPermlinkData(permlinkResp));
    // stdin がある場合は標準入力用のエディタを開く
    if (permlinkResp.parameter.stdin.length !== 0) {
      dispatch(actions.setStdinOpened(true));
    }
  }, [permlinkResp]);

  useEffect((): void => {
    doGetSponsors(null, {});
  }, []);

  const {
    permlinkData,
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
        permlinkData,
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
      permlinkData,
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

  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  // compilerList と permlinkData は state に保存しておく
  useEffect(() => {
    dispatch(actions.setCompilerList(compilerList));
  }, [compilerList]);

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
      dispatch(actions.pushQuickSave());
      dispatch(actions.setEditorChanged(false));
    }, 10000);
    return () => {
      clearTimeout(timer);
    };
  }, [editorChanged]);

  // 現在の breakpoint を設定する
  const updateBreakpoint = useCallback(() => {
    const breakpoints = ["xxl", "xl", "lg", "md", "sm", "xs"].map(
      (x) =>
        [
          x,
          parseInt(
            getComputedStyle(document.body).getPropertyValue(
              `--wb-breakpoint-${x}`
            ),
            10
          ),
        ] as [Breakpoint, number]
    );

    const width = document.documentElement.clientWidth;
    for (const [bp, value] of breakpoints) {
      if (width >= value) {
        dispatch(actions.setBreakpoint(bp));
        break;
      }
    }
  }, []);

  useEffect(() => {
    window.addEventListener("resize", updateBreakpoint);
    window.addEventListener("orientationchange", updateBreakpoint);
    return () => {
      window.removeEventListener("resize", updateBreakpoint);
      window.removeEventListener("orientationchange", updateBreakpoint);
    };
  }, [updateBreakpoint]);

  // 適切にタイトルを書き換える
  useEffect(() => {
    if (typeof document === "undefined") {
      return;
    }
    let language: string;
    let docTitle: string;
    if (permlinkResp === null) {
      language = currentLanguage;
      docTitle = title;
    } else {
      language = permlinkResp.parameter.compilerInfo.language;
      docTitle = permlinkResp.parameter.title;
    }
    document.title =
      (language.length === 0 ? "" : `[${language}] `) +
      (docTitle.length === 0 ? "" : `${docTitle} - `) +
      "Wandbox";
  }, [permlinkResp, currentLanguage, title]);

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
        contentClassName={`${
          sidebarLocked ? "wb-sidebar-locked" : ""
        } py-24px px-8px px-md-32px d-flex flex-column`}
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
            {sponsors !== null &&
              (sponsors.corporate.length !== 0 ||
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
