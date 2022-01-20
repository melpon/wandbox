import React, { createContext, useContext } from "react";

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

function useResultContextState(): ResultContextState {
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

const ResultContext = createContext<ResultContextState | null>(null);

function useResultContext(): ResultContextState {
  return useContext(ResultContext) as ResultContextState;
}

export { ResultContext, useResultContextState, useResultContext };
