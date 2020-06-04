import React from "react";
import { createContainer } from "unstated-next";

type ResultType =
  | "Control"
  | "CompilerMessageS"
  | "CompilerMessageE"
  | "StdOut"
  | "StdErr"
  | "ExitCode"
  | "Signal";

export interface ResultData {
  type: ResultType;
  data: string;
}

export interface ResultContextState {
  results: ResultData[];

  clear: () => void;
  add: (result: ResultData) => void;
  setResults: (results: ResultData[]) => void;
}

function useResultContext(): ResultContextState {
  const [results, setResults] = React.useState<ResultData[]>([]);
  const clear = React.useCallback((): void => setResults([]), []);
  const add = React.useCallback((result): void => {
    setResults((results): ResultData[] => [...results, result]);
  }, []);

  return {
    results,
    clear,
    add,
    setResults,
  };
}

const ResultContext = createContainer(useResultContext);
export { ResultContext };
