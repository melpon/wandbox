import ndjsonStream from "can-ndjson-stream";

import { CompilerContextState } from "~/contexts/CompilerContext";
import { EditorContextState } from "~/contexts/EditorContext";
import { CompilerList } from "~/hooks/compilerList";
import { AnyJson, JsonMap } from "~/hooks/fetch";
import { getCompileOptions } from "./getCompileOptions";

export function createBody(
  editor: EditorContextState,
  compiler: CompilerContextState,
  compilerList: CompilerList
): JsonMap | null {
  const defaultEditorTab = editor.sources.findIndex(
    (s): boolean => s.filename === null
  );
  if (defaultEditorTab === -1) {
    // something wrong
    return null;
  }

  const info = compilerList.compilers.find(
    (c): boolean => c.name === compiler.currentCompilerName
  );
  if (info === undefined) {
    return null;
  }

  // get options
  const options = getCompileOptions(compiler.currentSwitches, info);

  return {
    compiler: compiler.currentCompilerName,
    title: editor.title,
    description: editor.description,
    code: editor.getSourceText(defaultEditorTab),
    codes: editor.sources
      .map((s, tab) => ({ file: s.filename, code: editor.getSourceText(tab) }))
      .filter((x) => x.file !== null),
    options: options.join(","),
    stdin: editor.stdin,
    "compiler-option-raw": compiler.compilerOptionRaw,
    "runtime-option-raw": compiler.runtimeOptionRaw,
  };
}
export function compile(
  editor: EditorContextState,
  compiler: CompilerContextState,
  compilerList: CompilerList,
  onRead: (result: AnyJson) => void,
  onComplete: () => void,
  onError: (reason: any) => void
): void {
  const body = createBody(editor, compiler, compilerList);
  if (body === null) {
    return;
  }

  fetch(`${WANDBOX_URL_PREFIX}/api/compile.ndjson`, {
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