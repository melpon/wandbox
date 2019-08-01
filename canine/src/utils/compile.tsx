import ndjsonStream from "can-ndjson-stream";

import { CompilerContextState } from "~/contexts/CompilerContext";
import { EditorContextState } from "~/contexts/EditorContext";
import { CompilerList } from "~/hooks/compilerList";
import { AnyJson } from "~/hooks/fetch";
import { getCompileOptions } from "./getCompileOptions";

export function compile(
  editor: EditorContextState,
  compiler: CompilerContextState,
  compilerList: CompilerList,
  onRead: (result: AnyJson) => void
): void {
  const defaultEditor = editor.sources.find(
    (s): boolean => s.filename === null
  );
  if (defaultEditor === undefined) {
    // something wrong
    return;
  }

  const info = compilerList.compilers.find(
    (c): boolean => c.name === compiler.currentCompilerName
  );
  if (info === undefined) {
    return;
  }

  // get options
  const options = getCompileOptions(compiler.currentSwitches, info);

  const body = {
    compiler: compiler.currentCompilerName,
    code: defaultEditor.text,
    codes: editor.sources
      .filter((s): boolean => s.filename !== null)
      // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
      .map(s => ({ file: s.filename, code: s.text })),
    options: options,
    stdin: editor.stdin,
    "compiler-option-raw": compiler.compilerOptionRaw,
    "runtime-option-raw": compiler.runtimeOptionRaw,
    save: false
  };
  fetch("https://wandbox.org/api/compile.ndjson", {
    method: "POST",
    body: JSON.stringify(body),
    mode: "cors",
    headers: { "content-type": "application/json" }
  })
    .then(
      (resp): ReadableStream => {
        return ndjsonStream(resp.body);
      }
    )
    .then((stream): void => {
      const reader = stream.getReader();
      const read = (
        read: Function,
        result: ReadableStreamReadResult<AnyJson>
      ): void => {
        if (result.done) {
          return;
        }
        onRead(result.value);
        reader.read().then((result): void => read(read, result));
      };
      reader.read().then((result): void => read(read, result));
    });
}
