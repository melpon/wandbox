import React, { useCallback } from "react";
import { CompilerContextState } from "~/contexts/CompilerContext";
import { EditorContextState } from "~/contexts/EditorContext";
import { ResultContextState, ResultData } from "~/contexts/ResultContext";
import { SidebarContextState } from "~/contexts/SidebarContext";
import { compile } from "~/utils/compile";
import { CompilerList } from "./compilerList";
import { useError } from "./error";
import { AnyJson } from "./fetch";

export function useCompile(
  editor: EditorContextState,
  compiler: CompilerContextState,
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
  }, []);

  const [, setError] = useError();
  const onError = useCallback(
    (error) => {
      editor.setRunning(false);
      setError(error.toString());
    },
    [setError]
  );

  console.log("useCompile1", editor.sources);
  const doCompile = useCallback((): void => {
    result.clear();
    console.log("useCompile2", editor.sources);
    compile(editor, compiler, compilerList, onResult, onComplete, onError);
  }, [
    editor,
    result.clear,
    compiler,
    compilerList,
    onResult,
    onComplete,
    onError,
  ]);
  return doCompile;
}
