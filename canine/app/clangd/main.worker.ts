// 以下のソースをベースに改造している。
//
// https://github.com/guyutongxue/clangd-in-browser/blob/f4ad53f8d6cb8781417c26106290eaa8a619876b/src/main.worker.ts
//
// 元のライセンスは以下の通り。
//
// Copyright (c) 2024 Guyutongxue
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

/// <reference lib="WebWorker" />
import { BrowserMessageReader, BrowserMessageWriter } from 'vscode-jsonrpc/browser';

import { StdoutParser } from './StdoutParser';

export const LANGUAGE_ID = "cpp";
export const WORKSPACE_PATH = "/home/web_user";
export const FILE_PATH = "/home/web_user/main.cpp";

export const COMPILE_ARGS = [
  "-xc++",
  "-std=c++2b",
  "-pedantic-errors",
  "-Wall",
];

declare const self: DedicatedWorkerGlobalScope;

try {
  const wasmBase = `${import.meta.env.BASE_URL}static/wasm/`;
  const wasmUrls = [
    `${wasmBase}clangd.wasm.part001`,
    `${wasmBase}clangd.wasm.part002`,
    `${wasmBase}clangd.wasm.part003`,
    `${wasmBase}clangd.wasm.part004`,
  ];
  const jsModule = import(  /* @vite-ignore */ `${wasmBase}clangd.js`);

  // Pre-fetch wasm, and report progress to main
  let receivedLength = 0;
  const chunks: Uint8Array[] = [];
  const wasmSize = __WASM_SIZE__;
  for (const wasmUrl of wasmUrls) {
    const wasmResponse = await fetch(wasmUrl);
    const wasmReader = wasmResponse.body!.getReader();
    for (; ;) {
      const { done, value } = await wasmReader.read();
      if (done) {
        break;
      }
      if (value) {
        chunks.push(value);
        receivedLength += value.length;
        self.postMessage({
          type: "progress",
          value: receivedLength,
          max: Number(wasmSize),
        });
      }
    }
  }
  const wasmBlob = new Blob(chunks, { type: "application/wasm" });
  const wasmDataUrl = URL.createObjectURL(wasmBlob);

  const { default: Clangd } = await jsModule;

  const textEncoder = new TextEncoder();
  let resolveStdinReady = () => { };
  const stdinChunks: string[] = [];
  const currentStdinChunk: (number | null)[] = [];

  const stdin = (): number | null => {
    if (currentStdinChunk.length === 0) {
      if (stdinChunks.length === 0) {
        // Should not reach here
        // stdinChunks.push("Content-Length: 0\r\n", "\r\n");
        console.error("Try to fetch exhausted stdin");
        return null;
      }
      const nextChunk = stdinChunks.shift()!;
      currentStdinChunk.push(...textEncoder.encode(nextChunk), null);
    }
    return currentStdinChunk.shift()!;
  };

  const stdoutParser = new StdoutParser();
  stdoutParser.onRead = (message) => {
    console.log("[client <- server] %c%s", "color: green", JSON.stringify(message));
    writer.write(message);
  }
  const stdout = (charCode: number) => {
    stdoutParser.add(charCode);
  };

  let stderrLine = "";
  const stderr = (charCode: number) => {
    // if (charCode === LF) {
    //   console.log("%c%s", "color: darkorange", stderrLine);
    //   stderrLine = "";
    // } else {
    //   stderrLine += String.fromCharCode(charCode);
    // }
  };

  const stdinReady = async () => {
    if (stdinChunks.length === 0) {
      return new Promise<void>((r) => (resolveStdinReady = r));
    }
  };

  const onAbort = () => {
    writer.end();
    self.reportError("clangd aborted");
  };

  const clangd = await Clangd({
    thisProgram: "/usr/bin/clangd",
    locateFile: (path: string, prefix: string) => {
      return path.endsWith(".wasm") ? wasmDataUrl : `${prefix}${path}`;
    },
    stdinReady,
    stdin,
    stdout,
    stderr,
    onExit: onAbort,
    onAbort,
  });

  const flags = [
    ...COMPILE_ARGS,
    "--target=wasm32-wasi",
    "-isystem/usr/include/c++/v1",
    "-isystem/usr/include/wasm32-wasi/c++/v1",
    "-isystem/usr/include",
    "-isystem/usr/include/wasm32-wasi",
  ];

  clangd.FS.writeFile(FILE_PATH, "");
  clangd.FS.writeFile(
    `${WORKSPACE_PATH}/.clangd`,
    JSON.stringify({ CompileFlags: { Add: flags } })
  );

  console.log("%c%s", "font-size: 2em; color: green", "clangd started");
  clangd.callMain([]);

  self.postMessage({ type: "ready" });

  const reader = new BrowserMessageReader(self);
  const writer = new BrowserMessageWriter(self);

  reader.listen((data) => {
    // non-ASCII characters cause bad Content-Length. Just escape them.
    const body = JSON.stringify(data).replace(/[\u007F-\uFFFF]/g, (ch) => {
      return "\\u" + ch.codePointAt(0)!.toString(16).padStart(4, "0");
    });
    const header = `Content-Length: ${body.length}\r\n`;
    const delimiter = "\r\n";
    stdinChunks.push(header, delimiter, body);
    resolveStdinReady();
    console.log("[client -> server] %c%s", "color: red", `${header}${delimiter}${body}`);
  });
} catch (e) {
  self.postMessage({ type: "error", error: e });
}
