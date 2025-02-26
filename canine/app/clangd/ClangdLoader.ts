import clangdWorkerUrl from "~/clangd/main.worker?worker&url";

export class ClangdLoader {
  onError: (error: unknown) => void = () => { };
  onProgress: (value: number, max: number) => void = () => { };
  onReady: (worker: Worker) => void = () => { };

  async load(): Promise<void> {
    const worker = new Worker(clangdWorkerUrl, {
      type: 'module',
      name: 'Server Worker',
    });
    const promise = new Promise<void>((resolve, reject) => {
      const readyListener = (e: MessageEvent) => {
        switch (e.data?.type) {
          case "ready":
            this.onReady(worker);
            worker.removeEventListener("message", readyListener);
            resolve()
            break;
          case "progress":
            this.onProgress(e.data.value, e.data.max);
            break;
          case "error":
            this.onError(e.data.error);
            worker.removeEventListener("message", readyListener);
            reject(e.data.error);
            break;
        }
      };
      worker.addEventListener("message", readyListener);
    });
    return promise;
  }
}
