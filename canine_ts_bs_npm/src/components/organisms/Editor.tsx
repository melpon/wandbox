import React from "react";
import { useContainer } from "unstated-next";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { EditorContext } from "~/contexts/EditorContext";
import { CompilerContext } from "~/contexts/CompilerContext";
import { CompilerList } from "~/hooks/compilerList";
import { ResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { CodeEditor } from "./Editor/CodeEditor";
import { EditorSettings } from "./Editor/EditorSettings";
import { EditorTabs } from "./Editor/EditorTabs";

export interface EditorProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Editor: React.FC<EditorProps> = (props): React.ReactElement => {
  const editor = useContainer(EditorContext);
  const compiler = useContainer(CompilerContext);
  const result = useContainer(ResultContext);
  const { compilerList, permlinkData } = props;
  const { settings } = editor;
  return (
    <Row>
      <Col sm={settings.opened ? 10 : true}>
        <Row>
          <Col>
            <EditorTabs editor={editor} permlinkData={permlinkData} />
          </Col>
        </Row>
        <Row>
          <Col>
            <CodeEditor
              {...{ editor, compiler, compilerList, result, permlinkData }}
            />
          </Col>
        </Row>
      </Col>
      {((): React.ReactElement => {
        if (settings.opened) {
          return (
            <Col sm="2">
              <EditorSettings settings={settings} />
            </Col>
          );
        } else {
          return (
            <Col sm="auto">
              <EditorSettings settings={settings} />
            </Col>
          );
        }
      })()}
    </Row>
  );
};

export { Editor };
