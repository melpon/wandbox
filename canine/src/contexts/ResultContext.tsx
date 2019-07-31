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

export interface Result {
  type: ResultType;
  data: string;
}

export interface ResultContextState {
  results: Result[];

  clear: () => void;
  add: (result: Result) => void;
}

function useResultContext(): ResultContextState {
  const [results, setResults] = React.useState<Result[]>([]);
  const clear = React.useCallback((): void => setResults([]), []);
  const add = React.useCallback((result): void => {
    setResults((results): Result[] => [...results, result]);
  }, []);

  return {
    results,
    clear,
    add
  };
}

export const ResultContext = createContainer(useResultContext);
