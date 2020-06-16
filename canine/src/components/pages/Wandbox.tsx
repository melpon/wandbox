import React, { useEffect } from "react";
import useReactRouter from "use-react-router";
import Container from "react-bootstrap/Container";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { usePersistence } from "~/hooks/persistence";
import { Header } from "../organisms/Header";
import { Sidebar } from "../organisms/Sidebar";
import { Permlink } from "../organisms/Permlink";
import { Editor } from "../organisms/Editor";
import { Command } from "../organisms/Command";
import { Result } from "../organisms/Result";
import { Run } from "../organisms/Run";
import { EditorContext } from "~/contexts/EditorContext";
import { useContainer } from "unstated-next";
import { CompilerContext } from "~/contexts/CompilerContext";
import { ResultContext } from "~/contexts/ResultContext";

import "~/stylesheets/wandbox.scss";

interface WandboxRouterProps {
  permlinkId?: string;
}

const Wandbox: React.FC = (): React.ReactElement | null => {
  const { match } = useReactRouter<WandboxRouterProps>();
  const permlinkId =
    match.params.permlinkId === undefined ? null : match.params.permlinkId;
  const [, setError] = useError();
  const compilerList = useCompilerList(
    "https://wandbox.org/api/list.json",
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

    doGetPermlink(`https://wandbox.org/api/permlink/${permlinkId}`, {});
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

  // 設定データのロード（初回に一回だけ読み込む）
  const editor = useContainer(EditorContext);
  const compiler = useContainer(CompilerContext);
  const result = useContainer(ResultContext);
  const { load, save } = usePersistence(
    editor,
    compiler,
    result,
    permlinkId !== null
  );
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
    <Container fluid>
      <Row>
        <Header />
      </Row>
      <Row>
        <Col xs={12} sm={2}>
          <Sidebar compilerList={compilerList} permlinkData={permlinkData} />
        </Col>
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
      </Row>
    </Container>
  );
};

export { Wandbox };
