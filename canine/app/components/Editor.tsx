import React from "react";
import { useSelector } from "react-redux";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { CompilerList } from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { CodeEditor } from "./Editor/CodeEditor";
import { EditorSettings } from "./Editor/EditorSettings";
import { EditorTabs } from "./Editor/EditorTabs";
import { CodeMirror6 } from "./CodeMirror6";
import { Button } from "react-bootstrap";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { AppState, useAppDispatch, useAppStore } from "~/store";
import { wandboxSlice } from "~/features/slice";

export interface EditorProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Editor: React.FC<EditorProps> = (props): React.ReactElement => {
  const state = useSelector(({ wandbox }: AppState) => wandbox);
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const { compilerList, permlinkData } = props;
  const { editorSettings: settings } = state;
  // パーマリンク時は permlinkData からソースデータを作る
  const sources =
    permlinkData === null
      ? state.sources
      : createEditorSourceData(
          permlinkData.parameter.code,
          permlinkData.parameter.codes
        );
  return (
    <div className="d-flex justify-content-stretch gap-8px">
      <div className="d-flex flex-column flex-grow-1">
        <EditorTabs state={state} permlinkData={permlinkData} />
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
        {state.sources.map((source, tab) => {
          return (
            <CodeEditor
              key={source.id}
              {...{
                sources,
                tab,
                show: tab == state.currentTab,
                state,
                compilerList,
                permlinkData,
              }}
            />
          );
        })}
      </div>
      {state.stdinOpened && (
        <div className="d-flex flex-column">
          <Button
            variant="link"
            className="wb-stdinbutton wb-stdinactive align-self-end"
            onClick={() => dispatch(actions.setStdinOpened(!state.stdinOpened))}
          >
            Stdin
          </Button>
          <CodeMirror6
            className="wb-stdin flex-grow-1"
            initialText=""
            option={{}}
            onViewCreated={() => {}}
            onChange={() => {
              dispatch(actions.setSharable(false));
            }}
          />
        </div>
      )}
    </div>
  );
};

export { Editor };
