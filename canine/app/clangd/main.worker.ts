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
import { AsyncRunningQueue } from './AsyncRunningQueue';
import { getFromIndexedDB, saveToIndexedDB } from './ClangdBlob';

declare const self: DedicatedWorkerGlobalScope;
declare const __WASM_SIZE__: number;

let clangd: any;

const queue = new AsyncRunningQueue();

self.onmessage = (ev: MessageEvent) => {
  switch (ev.data.type) {
    case "init":
      queue.push(() => start(ev.data.workspace_path, ev.data.compiler_arguments));
      break;
  }
};

function write_compiler_arguments(clangd, workspace_path: string, compiler_arguments: string[]) {
  const flags = [
    ...compiler_arguments,
    "--target=wasm32-wasi-thread",
    "-isystem/usr/include/c++/v1",
    "-isystem/usr/include/wasm32-wasi-threads/c++/v1",
    "-isystem/usr/include",
    "-isystem/usr/include/wasm32-wasi-threads",
    "-pthread",
  ];

  // file:// で始まってたら消してあげる
  if (workspace_path.startsWith("file://")) {
    workspace_path = workspace_path.slice(7);
  }
  clangd.FS.writeFile(
    `${workspace_path}/.clangd`,
    JSON.stringify({ CompileFlags: { Add: flags } })
  );
}

async function start(workspace_path: string, compiler_arguments: string[]): Promise<void> {
  try {
    const wasmBase = `${import.meta.env.BASE_URL}static/wasm/`;
    const wasmUrls = [
      `${wasmBase}clangd.wasm`,
    ];
    const jsModule = import(  /* @vite-ignore */ `${wasmBase}clangd.js`);

    let wasmBlob: Blob | undefined = await getFromIndexedDB("clangd.wasm.v1");
    if (wasmBlob === undefined) {
      // Pre-fetch wasm, and report progress to main
      let receivedLength = 0;
      const chunks: Uint8Array[] = [];
      const wasmSize = __WASM_SIZE__;
      for (const wasmUrl of wasmUrls) {
        const wasmResponse = await fetch(wasmUrl, { headers: { "Accept-Encoding": "gzip" } });
        if (wasmResponse.status.toString()[0] !== "2") {
          throw new Error(`Failed to fetch ${wasmUrl}`);
        }
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
      wasmBlob = new Blob(chunks, { type: "application/wasm" });
      saveToIndexedDB("clangd.wasm.v1", wasmBlob);
    } else {
      self.postMessage({
        type: "progress",
        value: Number(__WASM_SIZE__),
        max: Number(__WASM_SIZE__),
      });
    }
    const wasmDataUrl = URL.createObjectURL(wasmBlob);

    const { default: Clangd } = await jsModule;

    const textEncoder = new TextEncoder();
    let resolveStdinReady = () => { };
    const stdinChunks: Uint8Array[] = [];
    const currentChunk: (number | null)[] = [];

    const stdin = (): number | null => {
      if (currentChunk.length === 0) {
        if (stdinChunks.length === 0) {
          return null;
        }
        currentChunk.push(...stdinChunks.shift()!, null);
      }
      return currentChunk.shift()!;
    };

    const stdoutParser = new StdoutParser();
    stdoutParser.onRead = (message) => {
      console.log("[client <- server] %c%s", "color: green", JSON.stringify(message));
      writer.write(message);
    }
    const stdout = (charCode: number) => {
      stdoutParser.add(charCode);
    };

    const stderrLine: number[] = [];
    const textDecoder = new TextDecoder("utf-8");
    const stderr = (charCode: number) => {
      if (charCode === 10) {
        const stderr = textDecoder.decode(new Uint8Array(stderrLine));
        console.log("%c%s", "color: darkorange", stderr);
        stderrLine.length = 0;
      } else {
        stderrLine.push(charCode);
      }
    };

    const stdinReady = async () => {
      if (stdinChunks.length === 0) {
        return new Promise<void>((r) => (resolveStdinReady = r));
      }
    };

    const onAbort = () => {
      self.reportError("clangd aborted");
      self.postMessage({ type: "error", error: "clangd aborted" });
      writer.end();
    };

    clangd = await Clangd({
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

    write_compiler_arguments(clangd, workspace_path, compiler_arguments);

    const reader = new BrowserMessageReader(self);
    const writer = new BrowserMessageWriter(self);

    console.log("%c%s", "font-size: 2em; color: green", "clangd started");
    clangd.callMain([]);

    self.postMessage({ type: "ready" });

    reader.listen((data) => {
      // 特殊なやつ
      switch (data.type) {
        case "change-compiler-arguments":
          queue.push(() => change_compiler_arguments(data.id, data.workspace_path, data.compiler_arguments));
          return;
      }

      const body = JSON.stringify(data);
      const utf8Body = textEncoder.encode(body);
      const header = `Content-Length: ${utf8Body.length}\r\n\r\n`;
      const utf8Header = textEncoder.encode(header);
      stdinChunks.push(utf8Header, utf8Body);
      resolveStdinReady();
      console.log("[client -> server] %c%s", "color: red", `${header}${body}`);
    });
  } catch (e) {
    self.postMessage({ type: "error", error: e.message });
  }

}

async function change_compiler_arguments(id: number, workspace_path: string, compiler_arguments: string[]): Promise<void> {
  try {
    write_compiler_arguments(clangd, workspace_path, compiler_arguments);
    self.postMessage({ type: "change-compiler-arguments-done", id });
  } catch (e) {
    self.postMessage({ type: "error", error: e.message, request_type: "change-compiler-arguments", id });
  }
}
