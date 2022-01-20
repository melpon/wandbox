import React from "react";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { EditorContext, useEditorContext } from "~/contexts/EditorContext";
import {
  CompilerContext,
  useCompilerContext,
} from "~/contexts/CompilerContext";
import { CompilerList } from "~/hooks/compilerList";
import { ResultContext, useResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { CodeEditor } from "./Editor/CodeEditor";
import { EditorSettings } from "./Editor/EditorSettings";
import { EditorTabs } from "./Editor/EditorTabs";
import { CodeMirror6 } from "./CodeMirror6";
import { Button } from "react-bootstrap";

export interface EditorProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Editor: React.FC<EditorProps> = (props): React.ReactElement => {
  const editor = useEditorContext();
  const compiler = useCompilerContext();
  const result = useResultContext();
  const { compilerList, permlinkData } = props;
  const { settings } = editor;
  return (
    <div className="d-flex justify-content-stretch gap-8px">
      <div className="d-flex flex-column flex-grow-1">
        <EditorTabs editor={editor} permlinkData={permlinkData} />
        {/*
          <CodeMirror6
            className="flex-grow-1"
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
          */}
        {editor.sources.map((_, tab) => {
          return (
            <CodeEditor
              {...{
                tab,
                show: tab == editor.currentTab,
                editor,
                compiler,
                compilerList,
                result,
                permlinkData,
              }}
            />
          );
        })}
      </div>
      {editor.stdinOpened && (
        <div className="d-flex flex-column">
          <Button
            variant="link"
            className="wb-stdinbutton wb-stdinactive align-self-end"
            onClick={() => editor.setStdinOpened(!editor.stdinOpened)}
          >
            Stdin
          </Button>
          <CodeMirror6
            className="wb-stdin flex-grow-1"
            initialText=""
            option={{}}
            onViewCreated={() => {}}
          />
        </div>
      )}
    </div>
  );
};

export { Editor };
