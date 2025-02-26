import React, { useCallback, useEffect, useRef, useState } from "react";
import { ClangdWorkerState, wandboxSlice } from "~/features/slice";
import { AppState, useAppDispatch } from "~/store";
import { useSelector } from "react-redux";
import { useTranslation } from "react-i18next";

import { ClangdLoader } from "~/clangd/ClangdLoader";
import { LanguageServerClient, languageServerWithTransport } from "~/clangd/codemirror-languageserver";
import { BrowserMessageReader, BrowserMessageWriter, createMessageConnection } from "vscode-jsonrpc/browser";
import { Button } from "react-bootstrap";

interface ClangdWorkerProps {
  clangdWorker: Worker | null;
  clangdWorkerState: ClangdWorkerState;
  clangdWorkerStatus: string;
}

const ClangdWorker: React.FC<ClangdWorkerProps> = ({ clangdWorker, clangdWorkerState, clangdWorkerStatus }): React.ReactElement => {
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  const loadClangdWorker = useCallback(() => {
    dispatch(actions.setClangdWorkerState("loading"));

    const loader = new ClangdLoader();
    loader.onReady = (worker) => {
      dispatch(actions.setClangdWorkerStatus("[clangd] Ready"));
      dispatch(actions.setClangdWorker(worker));
      dispatch(actions.setClangdWorkerState("ready"));
    };
    loader.onProgress = (value, max) => {
      dispatch(actions.setClangdWorkerStatus("[clangd] Loading... " + (value / max) * 100 + " %"));
    };
    loader.onError = (error) => {
      dispatch(actions.setClangdWorkerStatus("[clangd] Loading failed"));
      dispatch(actions.setClangdWorkerState("error"));
      console.error(error);
    };
    loader.load();
  }, []);

  const unloadClangdWorker = useCallback(() => {
    clangdWorker?.terminate();
    dispatch(actions.setClangdWorker(null));
    dispatch(actions.setClangdWorkerState("initial"));
    dispatch(actions.setClangdWorkerStatus("[clangd] Unloaded"));
  }, []);

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
    <div className="d-flex flex-row justify-content-end">
      <p>{clangdWorkerStatus}</p>
      <Button hidden={clangdWorkerState !== "initial"} onClick={loadClangdWorker}>Enable LSP</Button>
      <Button hidden={clangdWorkerState === "initial" || clangdWorkerState === "loading"} variant="danger" onClick={unloadClangdWorker}>Disable LSP</Button>
    </div>
  );
}

export { ClangdWorker };
