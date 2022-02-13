import React, { useContext, useEffect } from "react";
import { useSelector } from "react-redux";
import Container from "react-bootstrap/Container";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";
import { formatDistanceToNow } from "date-fns";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { usePersistence } from "~/hooks/persistence";
import { Header } from "~/components/Header";
import { Compiler } from "~/components/Compiler";
import { Permlink } from "~/components/Permlink";
import { Editor } from "~/components/Editor";
import { Command } from "~/components/Command";
import { Result } from "~/components/Result";
import { Run } from "~/components/Run";
import { Title } from "~/components/Title";
import { Button } from "react-bootstrap";
import { useParams } from "remix";
import { Author } from "~/components/Author";
import Sidebar from "~/components/react-sidebar/Sidebar";
import { useSidebarContext } from "~/contexts/SidebarContext";
import { Gear, LayoutSidebarReverse, X } from "react-bootstrap-icons";
import { AppState, useAppDispatch, useAppStore } from "~/store";

interface WandboxRouterProps {
  permlinkId?: string;
}

const Wandbox: React.FC = (): React.ReactElement | null => {
  const { permlinkId } = useParams();
  const [, setError] = useError();
  const compilerList = useCompilerList(
    `${WANDBOX_URL_PREFIX}/api/list.json`,
    setError
  );

  const [permlinkData, setPermlinkData] = React.useState<PermlinkData | null>(
    null
  );
  const [permlinkResp, , doGetPermlink] = useGetPermlink(
    permlinkId === undefined ? "" : permlinkId,
    setError
  );

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
  }, [permlinkResp]);

  const clearPermlinkData = React.useCallback((): void => {
    setPermlinkData(null);
  }, [setPermlinkData]);

  const state = useSelector(({ wandbox }: AppState) => wandbox);
  const dispatch = useAppDispatch();
  const sidebar = useSidebarContext();
  const { load, save } = usePersistence(
    dispatch,
    state,
    sidebar,
    permlinkId !== undefined
  );
  // 設定データのロード（初回に一回だけ読み込む）
  React.useEffect((): void => {
    load();
  }, []);

  // データに変化があった3秒後に設定を保存する
  React.useEffect((): (() => void) => {
    const timerID = setTimeout((): void => {
      save();
    }, 3000);

    return (): void => {
      clearTimeout(timerID);
    };
  }, [save]);
  // それとは別に、設定周りの変更があったら即座に保存する
  React.useEffect((): void => {
    save();
  }, [state.editorSettings]);

  if (compilerList === null) {
    return null;
  }

  const sidebarContent = (
    <div
      style={{ width: 244, height: "100%", backgroundColor: "white" }}
      className="d-flex flex-column"
    >
      <div
        className="d-flex justify-content-between px-8px py-4px"
        style={{ borderBottom: "#d0d7de solid 1px" }}
      >
        <div className="d-flex align-items-center">
          <h5>History</h5>
          <Button
            variant="link"
            active={sidebar.locked}
            onClick={() => {
              sidebar.setLocked(!sidebar.locked);
            }}
          >
            <LayoutSidebarReverse />
          </Button>
        </div>
        <Button
          variant="link"
          onClick={() => {
            sidebar.setOpened(false);
            sidebar.setLocked(false);
          }}
        >
          <X />
        </Button>
      </div>
      <div className="flex-grow-1">
        {sidebar.history.data.histories.map((x) => {
          if (x.type === "permlink") {
            return (
              <div
                key={`wb-history-${x.id}`}
                className="d-flex flex-column"
                style={{ border: "#eeeeee solid 1px" }}
              >
                <p>
                  {formatDistanceToNow(x.createdAt * 1000, { addSuffix: true })}
                </p>
                <p>{x.permlinkId}</p>
                <p>{x.title}</p>
                <p>{x.currentLanguage}</p>
                <p>{x.currentCompilerName}</p>
                <p>
                  {formatDistanceToNow(x.permlinkCreatedAt * 1000, {
                    addSuffix: true,
                  })}
                </p>
                {x.githubUser && <p>{`@${x.githubUser.login}`}</p>}
              </div>
            );
          }
        })}
      </div>
    </div>
  );

  return (
    <div id="wb-main" className="d-flex flex-column">
      <Header />
      <Sidebar
        open={sidebar.opened}
        docked={sidebar.locked}
        sidebar={sidebarContent}
        onSetOpen={sidebar.setOpened}
        pullRight={true}
        rootClassName="wb-sidebar"
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
          {(state.currentCompilerName !== "" || permlinkData !== null) && (
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
