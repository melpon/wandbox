import React, { useCallback, useEffect, useMemo } from "react";
import { useSelector } from "react-redux";
import { KeyBinding } from "@codemirror/view";

import { CompilerList } from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { CodeEditor } from "./Editor/CodeEditor";
import { EditorTabs } from "./Editor/EditorTabs";
import { CodeMirror6, CodeMirror6Option } from "./CodeMirror6";
import { Button } from "react-bootstrap";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { AppState, useAppDispatch, useAppStore } from "~/store";
import { wandboxSlice } from "~/features/slice";
import { useCompileStateSelector } from "~/utils/compile";
import { useCompile } from "~/hooks/compile";

export interface EditorProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Editor: React.FC<EditorProps> = (props) => {
  const {
    currentLanguage,
    sources,
    stdin,
    stdinOpened,
    stdinView,
    currentTab,
    tabKey,
    tabWidth,
    fixedHeight,
  } = useSelector(
    ({
      wandbox: {
        currentLanguage,
        sources,
        stdin,
        stdinOpened,
        stdinView,
        currentTab,
        editorSettings: { tabKey, tabWidth, fixedHeight },
      },
    }: AppState) => ({
      currentLanguage,
      sources,
      stdin,
      stdinOpened,
      stdinView,
      currentTab,
      tabKey,
      tabWidth,
      fixedHeight,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const { compilerList, permlinkData } = props;
  // パーマリンク時は permlinkData からソースデータを作る
  const resolvedSources =
    permlinkData === null
      ? sources
      : createEditorSourceData(
          permlinkData.parameter.code,
          permlinkData.parameter.codes
        );

  const compileState = useCompileStateSelector();
  const doCompile = useCompile(dispatch, compileState, compilerList);
  const onRun = useCallback(() => {
    dispatch(actions.setRunning(true));
    dispatch(actions.setSharable(true));
    dispatch(actions.prepareRun());
    doCompile();
  }, [doCompile]);

  // stdin を閉じる際に view の内容を stdin に反映する
  useEffect(() => {
    if (permlinkData !== null || stdinView === undefined || stdinOpened) {
      return;
    }
    dispatch(actions.setStdin(stdinView.state.doc.toString()));
  }, [stdinView, stdinOpened]);

  const ctrlEnter = useMemo((): KeyBinding => {
    return {
      key: "Ctrl-Enter",
      run: () => {
        onRun();
        return true;
      },
    };
  }, [onRun]);
  const option = useMemo((): CodeMirror6Option => {
    return {
      lineNumbers: true,
      tabSize: parseInt(tabWidth, 10),
      indentUnit: tabKey !== "tab" ? parseInt(tabKey, 10) : undefined,
      indentWithTab: tabKey === "tab",
      readOnly: permlinkData !== null,
      keymaps: [ctrlEnter],
    };
  }, [tabWidth, tabKey, permlinkData, ctrlEnter]);

  return (
    <div
      className={`${
        fixedHeight ? "wb-editor-fixedheight" : ""
      } d-flex flex-column flex-md-row gap-8px`}
    >
      <div className="d-flex flex-column flex-grow-1">
        <EditorTabs
          currentTab={currentTab}
          stdinOpened={stdinOpened}
          sources={resolvedSources}
          permlinkData={permlinkData}
        />
        {resolvedSources.map((source, tab) => {
          return (
            <CodeEditor
              key={source.id}
              source={source}
              tab={tab}
              compilerList={compilerList}
              permlinkData={permlinkData}
            />
          );
        })}
      </div>
      {!stdinOpened && (
        <Button
          variant="link"
          className="wb-stdinbutton d-block d-md-none align-self-start align-self-md-end"
          onClick={() => dispatch(actions.setStdinOpened(!stdinOpened))}
        >
          Stdin
        </Button>
      )}
      {stdinOpened && (
        <div className="d-flex flex-column">
          <Button
            variant="link"
            className="wb-stdinbutton wb-stdinactive align-self-start align-self-md-end"
            onClick={() => dispatch(actions.setStdinOpened(!stdinOpened))}
          >
            Stdin
          </Button>
          <CodeMirror6
            className="wb-stdin flex-grow-1"
            text={permlinkData === null ? stdin : permlinkData.parameter.stdin}
            option={option}
            onViewCreated={(view) => {
              if (permlinkData === null) {
                dispatch(actions.setStdinView(view));
              }
            }}
            onViewDestroyed={(view) => {}}
            onChange={() => {
              dispatch(actions.setSharable(false));
              dispatch(actions.setEditorChanged(true));
            }}
          />
        </div>
      )}
    </div>
  );
};

export { Editor };
