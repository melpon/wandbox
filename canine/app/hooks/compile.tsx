import React, { useCallback } from "react";
import { EditorContextState } from "~/contexts/EditorContext";
import { ResultContextState, ResultData } from "~/contexts/ResultContext";
import { SidebarContextState } from "~/contexts/SidebarContext";
import { WandboxState } from "~/features/slice";
import { compile } from "~/utils/compile";
import { CompilerList } from "./compilerList";
import { useError } from "./error";
import { AnyJson } from "./fetch";

export function useCompile(
  editor: EditorContextState,
  state: WandboxState,
  sidebar: SidebarContextState,
  compilerList: CompilerList,
  result: ResultContextState
): () => void {
  const onResult = useCallback(
    (json: AnyJson): void => {
      result.add(json as unknown as ResultData);
    },
    [result.add]
  );

  const onComplete = useCallback(() => {
    editor.setRunning(false);
    sidebar.history.commitRun(result);
  }, [sidebar, editor, result]);

  const [, setError] = useError();
  const onError = useCallback(
    (error) => {
      editor.setRunning(false);
      setError(error.toString());
    },
    [editor, setError]
  );

  const doCompile = useCallback((): void => {
    result.clear();
    compile(editor, state, compilerList, onResult, onComplete, onError);
  }, [
    editor,
    result.clear,
    state,
    compilerList,
    onResult,
    onComplete,
    onError,
  ]);
  return doCompile;
}
