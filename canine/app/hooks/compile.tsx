import React, { useCallback } from "react";
import { EditorContextState } from "~/contexts/EditorContext";
import { SidebarContextState } from "~/contexts/SidebarContext";
import { ResultData, wandboxSlice, WandboxState } from "~/features/slice";
import { AppDispatch } from "~/store";
import { compile } from "~/utils/compile";
import { CompilerList } from "./compilerList";
import { useError } from "./error";
import { AnyJson } from "./fetch";

export function useCompile(
  dispatch: AppDispatch,
  editor: EditorContextState,
  state: WandboxState,
  sidebar: SidebarContextState,
  compilerList: CompilerList
): () => void {
  const actions = wandboxSlice.actions;
  const onResult = useCallback((json: AnyJson): void => {
    dispatch(actions.addResult(json as unknown as ResultData));
  }, []);

  const onComplete = useCallback(() => {
    editor.setRunning(false);
    sidebar.history.commitRun(state);
  }, [sidebar, editor, state]);

  const [, setError] = useError();
  const onError = useCallback(
    (error) => {
      editor.setRunning(false);
      setError(error.toString());
    },
    [editor, setError]
  );

  const doCompile = useCallback((): void => {
    dispatch(actions.clearResult());
    compile(editor, state, compilerList, onResult, onComplete, onError);
  }, [editor, state, compilerList, onResult, onComplete, onError]);
  return doCompile;
}
