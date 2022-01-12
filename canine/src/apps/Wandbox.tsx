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

  // 設定データのロード（初回に一回だけ読み込む）
  const editor = useEditorContext();
  const compiler = useCompilerContext();
  const result = useResultContext();
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
            <CodeMirror6
              initialText="Hello"
              option={{
                tabSize: parseInt(editor.settings.tabWidth, 10),
                indentUnit:
                  editor.settings.tabKey !== "tab"
                    ? parseInt(editor.settings.tabKey, 10)
                    : undefined,
                indentWithTabs: editor.settings.tabKey === "tab",
              }}
              onViewCreated={() => {}}
            />
          </Row>
          <Row>
            <EditorSettings settings={editor.settings} />
          </Row>
        </Col>
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
      </Row>
    </Container>
  );
};

export { Wandbox };
