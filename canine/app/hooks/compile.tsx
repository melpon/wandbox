import { useCallback } from "react";

import type { ResultData } from "~/features/slice";
import { wandboxSlice } from "~/features/slice";
import type { AppDispatch } from "~/store";
import type { CompileState } from "~/utils/compile";
import { compile } from "~/utils/compile";
import type { CompilerList } from "./compilerList";
import { useError } from "./error";
import type { AnyJson } from "./fetch";

export function useCompile(
  dispatch: AppDispatch,
  state: CompileState,
  compilerList: CompilerList
): () => void {
  const actions = wandboxSlice.actions;
  const onResult = useCallback((json: AnyJson): void => {
    dispatch(actions.addResult(json as unknown as ResultData));
  }, []);

  const onComplete = useCallback(() => {
    dispatch(actions.setRunning(false));
    dispatch(actions.commitRun());
  }, []);

  const [, setError] = useError();
  const onError = useCallback(
    (error) => {
      dispatch(actions.setRunning(false));
      setError(error.toString());
    },
    [setError]
  );

  const doCompile = useCallback((): void => {
    dispatch(actions.clearResult());
    compile(state, compilerList, onResult, onComplete, onError);
  }, [state, compilerList, onResult, onComplete, onError]);
  return doCompile;
}
