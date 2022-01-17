import React, { useContext, useEffect } from "react";
import Container from "react-bootstrap/Container";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { usePersistence } from "~/hooks/persistence";
import { Header } from "~/components/Header";
import { Sidebar } from "~/components/Sidebar";
import { Permlink } from "~/components/Permlink";
import { Editor } from "~/components/Editor";
import { Command } from "~/components/Command";
import { Result } from "~/components/Result";
import { Run } from "~/components/Run";
import { CodeMirror6 } from "~/components/CodeMirror6";
import { useEditorContext } from "~/contexts/EditorContext";
import { useCompilerContext } from "~/contexts/CompilerContext";
import { useResultContext } from "~/contexts/ResultContext";
import { EditorSettings } from "~/components/Editor/EditorSettings";
import { Title } from "~/components/Title";
import { Button } from "react-bootstrap";

interface WandboxRouterProps {
  permlinkId?: string;
}

const Wandbox: React.FC = (): React.ReactElement | null => {
  //const permlinkId =
  //  match.params.permlinkId === undefined ? null : match.params.permlinkId;
  const permlinkId: string | null = null;
  const [, setError] = useError();
  const compilerList = useCompilerList(
    `${process.env.NEXT_PUBLIC_WANDBOX_URL_PREFIX}/api/list.json`,
    setError
  );

  const [permlinkData, setPermlinkData] = React.useState<PermlinkData | null>(
    null
  );
  const [permlinkResp, , doGetPermlink] = useGetPermlink(
    permlinkId === null ? "" : permlinkId,
    setError
  );

  useEffect((): void => {
    if (permlinkId === null) {
      setPermlinkData(null);
      return;
    }

    doGetPermlink(
      `${process.env.NEXT_PUBLIC_WANDBOX_URL_PREFIX}/api/permlink/${permlinkId}`,
      {}
    );
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

  const editor = useEditorContext();
  const compiler = useCompilerContext();
  const result = useResultContext();
  const { load, save } = usePersistence(
    editor,
    compiler,
    result,
    permlinkId !== null
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
  }, [compiler, editor.settings]);

  if (compilerList === null) {
    return null;
  }

  return (
    <div id="wb-main" className="d-flex flex-column">
      <Header />
      <div className="pt-24px px-16px d-flex gap-16px">
        <Sidebar compilerList={compilerList} permlinkData={permlinkData} />
        <div className="flex-grow-1 d-flex flex-column gap-8px">
          <Title />
          <Editor compilerList={compilerList} permlinkData={permlinkData} />
          <Command compilerList={compilerList} permlinkData={permlinkData} />
          <Run compilerList={compilerList} permlinkData={permlinkData} />
          <Result permlinkData={permlinkData} />
        </div>
        {/*
        <Col xs={12} sm={10}>
          <Row>
            <Col>
              <Permlink
                compilerList={compilerList}
                permlinkData={permlinkData}
                clearPermlinkData={clearPermlinkData}
              />
            </Col>
          </Row>
          <Row>
            <Col>
              <Editor compilerList={compilerList} permlinkData={permlinkData} />
            </Col>
          </Row>
          <Row>
            <Col sm="auto">
              <Run compilerList={compilerList} permlinkData={permlinkData} />
            </Col>
            <Col sm="auto">
              <Command
                compilerList={compilerList}
                permlinkData={permlinkData}
              />
            </Col>
          </Row>
          <Row>
            <Col>
              <Result permlinkData={permlinkData} />
            </Col>
          </Row>
        </Col>
        */}
      </div>
    </div>
  );
};

export { Wandbox };
