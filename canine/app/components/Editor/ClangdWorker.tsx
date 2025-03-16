import React, { useCallback, useEffect, useRef, useState } from "react";
import { ClangdWorkerState, wandboxSlice } from "~/features/slice";
import { AppState, useAppDispatch } from "~/store";
import { useSelector } from "react-redux";
import { useTranslation } from "react-i18next";

import { ClangdServer } from "~/clangd/ClangdServer";
import { LanguageServerClient, languageServerWithTransport } from "~/clangd/codemirror-languageserver";
import { BrowserMessageReader, BrowserMessageWriter, createMessageConnection } from "vscode-jsonrpc/browser";
import { Button } from "react-bootstrap";

interface ClangdWorkerProps {
  clangdWorker: Worker | null;
  clangdWorkerState: ClangdWorkerState;
  clangdWorkerStatus: string;
  clangdClient: LanguageServerClient | null;
  clangdServer: ClangdServer | null;
  currentSwitches: { [name: string]: string | boolean };
}

export const COMPILE_ARGS = [
  "-xc++",
  "-std=c++2b",
  "-pedantic-errors",
  "-Wall",
];

function switchesToOptions(currentSwitches: { [name: string]: string | boolean }) {
  const args: string[] = []
  if (currentSwitches["warning"]) {
    args.push("-Wall", "-Wextra");
  }
  switch (currentSwitches["std-cxx"]) {
    case "gnu++2b":
      args.push("-std=c++2b");
      break;
    case "c++2b":
      args.push("-std=c++2b");
      break;
    case "gnu++2a":
      args.push("-std=c++2a");
      break;
    case "c++2a":
      args.push("-std=c++2a");
      break;
    case "gnu++17":
      args.push("-std=c++17");
      break;
    case "c++17":
      args.push("-std=c++17");
      break;
    case "gnu++14":
      args.push("-std=c++14");
      break;
    case "c++14":
      args.push("-std=c++14");
      break;
    case "gnu++11":
      args.push("-std=c++11");
      break;
    case "c++11":
      args.push("-std=c++11");
      break;
    case "gnu++03":
      args.push("-std=c++03");
      break;
    case "c++03":
      args.push("-std=c++03");
      break;
  }
  switch (currentSwitches["cpp-pedantic"]) {
    case "cpp-pedantic-errors":
      args.push("-pedantic-errors");
      break;
    case "cpp-pedantic":
      args.push("-pedantic-errors");
      break;
  }
  args.push("-isystem/usr/local/include/wandbox");
  args.push("-I/usr/local/include/wandbox/hpplib");
  // mmap を使ってるプロジェクトをコンパイルするために必要
  args.push("-D_WASI_EMULATED_MMAN");

  return args;
}

function getDifference<T>(x: T[], y: T[]): { add: T[]; remove: T[] } {
  const setX = new Set(x);
  const setY = new Set(y);

  const add = y.filter(item => !setX.has(item));
  const remove = x.filter(item => !setY.has(item));

  return { add, remove };
}

const ClangdWorker: React.FC<ClangdWorkerProps> = ({ clangdWorker, clangdWorkerState, clangdWorkerStatus, clangdClient, clangdServer, currentSwitches }): React.ReactElement => {
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  const [oldCurrentSwitches, setOldCurrentSwitches] = useState<{ [name: string]: string | boolean }[]>([]);
  useEffect(() => {
    let xs = [...oldCurrentSwitches, currentSwitches];
    if (xs.length > 2) {
      xs = xs.slice(1);
    }
    setOldCurrentSwitches(xs);
  }, [currentSwitches]);

  const loadClangdWorker = useCallback(() => {
    dispatch(actions.setClangdWorkerState("loading"));

    const server = new ClangdServer();
    server.onReady = (worker) => {
      dispatch(actions.setClangdWorkerStatus("[clangd] Ready"));
      dispatch(actions.setClangdWorker(worker));
      dispatch(actions.setClangdWorkerState("ready"));
    };
    server.onProgress = (value, max) => {
      if (value !== max) {
        dispatch(actions.setClangdWorkerStatus("[clangd] Downloading... " + Math.floor((value / max) * 100) + " %"));
      } else {
        dispatch(actions.setClangdWorkerStatus("[clangd] Starting..."));
      }
    };
    server.onError = (error) => {
      // dispatch(actions.setClangdWorkerStatus("[clangd] Error"));
      // dispatch(actions.setClangdWorkerState("error"));
      console.error(error);
      dispatch(actions.setClangdWorkerStatus("[clangd] Restarting..."));
      dispatch(actions.setClangdWorkerState("error"));
    };
    server.start('file:///home/web_user/', switchesToOptions(currentSwitches));
    dispatch(actions.setClangdServer(server));
  }, [currentSwitches]);

  // エラーが起きたら再起動する
  useEffect(() => {
    if (clangdWorkerState !== "error" || clangdServer === null) {
      return;
    }
    clangdClient?.close();
    clangdWorker?.terminate();
    dispatch(actions.setClangdWorkerState("initial"));
    dispatch(actions.setClangdWorker(null));
    dispatch(actions.setClangdClient(null));
    clangdServer.start('file:///home/web_user/', switchesToOptions(currentSwitches));
  }, [clangdWorkerState, clangdClient, clangdWorker, clangdServer, currentSwitches]);

  useEffect(() => {
    if (clangdServer === null || clangdClient === null) {
      return;
    }
    clangdServer.onUpdate = () => {
      // clangdClient.update();
      if (oldCurrentSwitches.length < 2) {
        return;
      }
      // oldCurrentSwitches と currentSwitches の差分を送る
      const optDiff = getDifference(switchesToOptions(oldCurrentSwitches[0]), switchesToOptions(oldCurrentSwitches[1]));
      clangdClient.workspaceDidChangeConfiguration({
        settings: {
          clangd: {
            compileFlags: {
              add: optDiff.add,
              remove: optDiff.remove,
            },
          },
        },
      })
      clangdClient.update();
    };
  }, [clangdClient, clangdServer, oldCurrentSwitches]);

  useEffect(() => {
    if (clangdServer === null) {
      return;
    }
    clangdServer.change_compiler_arguments('file:///home/web_user/', switchesToOptions(currentSwitches));
  }, [currentSwitches]);

  const unloadClangdWorker = useCallback(() => {
    clangdClient?.close();
    clangdWorker?.terminate();
    dispatch(actions.setClangdWorker(null));
    dispatch(actions.setClangdClient(null));
    dispatch(actions.setClangdServer(null));
    dispatch(actions.setClangdWorkerState("initial"));
    dispatch(actions.setClangdWorkerStatus("[clangd] Unloaded"));
  }, [clangdClient, clangdWorker]);

  useEffect(() => {
    if (clangdWorker === null) {
      dispatch(actions.setClangdClient(null));
      return;
    }
    const client = new LanguageServerClient({
      rootUri: 'file:///home/web_user',
      workspaceFolders: [{
        uri: 'file:///home/web_user',
        name: 'wandbox',
      }],
      connection: createMessageConnection(new BrowserMessageReader(clangdWorker), new BrowserMessageWriter(clangdWorker)),
    });
    dispatch(actions.setClangdClient(client));
  }, [clangdWorker]);

  return (
    <div className="d-flex flex-row justify-content-end gap-8px mt-4px">
      <p>{clangdWorkerStatus}</p>
      <Button hidden={clangdWorkerState !== "initial"} onClick={loadClangdWorker}>Enable LSP</Button>
      <Button hidden={clangdWorkerState === "initial" || clangdWorkerState === "loading"} variant="danger" onClick={unloadClangdWorker}>Disable LSP</Button>
    </div>
  );
}

export { ClangdWorker };
