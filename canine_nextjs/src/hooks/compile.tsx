import React from "react";
import { CompilerContextState } from "~/contexts/CompilerContext";
import { EditorContextState } from "~/contexts/EditorContext";
import { ResultContextState, ResultData } from "~/contexts/ResultContext";
import { compile } from "~/utils/compile";
import { CompilerList } from "./compilerList";
import { AnyJson } from "./fetch";

export function useCompile(
  editor: EditorContextState,
  compiler: CompilerContextState,
  compilerList: CompilerList,
  result: ResultContextState
): () => void {
  const onResult = React.useCallback(
    (json: AnyJson): void => {
      result.add((json as unknown) as ResultData);
    },
    [result.add]
  );
  const doCompile = React.useCallback((): void => {
    result.clear();
    compile(editor, compiler, compilerList, onResult);
  }, [editor, result.clear, compiler, compilerList, onResult]);
  return doCompile;
}
