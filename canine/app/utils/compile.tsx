import { EditorView } from "@codemirror/view";
import { createSelector } from "@reduxjs/toolkit";
import ndjsonStream from "can-ndjson-stream";
import { useSelector } from "react-redux";

import { getSourceText } from "~/features/actions";
import { EditorSourceData, getStdin, WandboxState } from "~/features/slice";
import { CompilerList } from "~/hooks/compilerList";
import { AnyJson, JsonMap } from "~/hooks/fetch";
import { AppState } from "~/store";
import { getCompileOptions } from "./getCompileOptions";

export type CompileState = {
  currentCompilerName: string;
  currentSwitches: { [name: string]: string | boolean };
  compilerOptionRaw: string;
  runtimeOptionRaw: string;
  title: string;
  description: string;
  sources: EditorSourceData[];
  stdin: string;
  stdinView?: EditorView;
};

const compileStateSelector = createSelector(
  ({ wandbox: { currentCompilerName } }: AppState) => currentCompilerName,
  ({ wandbox: { currentSwitches } }: AppState) => currentSwitches,
  ({ wandbox: { compilerOptionRaw } }: AppState) => compilerOptionRaw,
  ({ wandbox: { runtimeOptionRaw } }: AppState) => runtimeOptionRaw,
  ({ wandbox: { title } }: AppState) => title,
  ({ wandbox: { description } }: AppState) => description,
  ({ wandbox: { sources } }: AppState) => sources,
  ({ wandbox: { stdin } }: AppState) => stdin,
  ({ wandbox: { stdinView } }: AppState) => stdinView,
  (
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    title,
    description,
    sources,
    stdin,
    stdinView
  ) => ({
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    title,
    description,
    sources,
    stdin,
    stdinView,
  })
);

export function useCompileStateSelector(): CompileState {
  return useSelector(compileStateSelector);
}

export function createBody(
  state: CompileState,
  compilerList: CompilerList
): JsonMap | null {
  const defaultEditorTab = state.sources.findIndex(
    (s): boolean => s.filename === null
  );
  if (defaultEditorTab === -1) {
    // something wrong
    return null;
  }

  const info = compilerList.compilers.find(
    (c): boolean => c.name === state.currentCompilerName
  );
  if (info === undefined) {
    return null;
  }

  // get options
  const options = getCompileOptions(state.currentSwitches, info);

  return {
    compiler: state.currentCompilerName,
    title: state.title,
    description: state.description,
    code: getSourceText(state.sources[defaultEditorTab]),
    codes: state.sources
      .map((s, tab) => ({ file: s.filename, code: getSourceText(s) }))
      .filter((x) => x.file !== null),
    options: options.join(","),
    stdin: getStdin(state.stdin, state.stdinView),
    "compiler-option-raw": state.compilerOptionRaw,
    "runtime-option-raw": state.runtimeOptionRaw,
  };
}

export function compile(
  state: CompileState,
  compilerList: CompilerList,
  onRead: (result: AnyJson) => void,
  onComplete: () => void,
  onError: (reason: any) => void
): void {
  const body = createBody(state, compilerList);
  if (body === null) {
    return;
  }

  fetch(`/api/compile.ndjson`, {
    method: "POST",
    body: JSON.stringify(body),
    mode: "cors",
    headers: { "content-type": "application/json" },
  })
    .then((resp): ReadableStream => {
      return ndjsonStream(resp.body);
    })
    .then((stream): void => {
      const reader = stream.getReader();
      const read = (
        // eslint-disable-next-line @typescript-eslint/ban-types
        read: Function,
        result: ReadableStreamDefaultReadResult<AnyJson>
      ): void => {
        if (result.done) {
          onComplete();
          return;
        }
        onRead(result.value);
        reader.read().then((result): void => read(read, result));
      };
      reader.read().then((result): void => read(read, result));
    })
    .catch((reason) => {
      onError(reason);
    });
}
