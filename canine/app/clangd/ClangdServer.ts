import clangdWorkerUrl from "~/clangd/main.worker?worker&url";

export class ClangdServer {
  onError: (error: unknown) => void = () => { };
  onProgress: (value: number, max: number) => void = () => { };
  onUpdate: (data: any) => void = () => { };
  onReady: (worker: Worker) => void = () => { };
  worker: Worker | null = null;
  #requestId: number = 0;

  async start(workspace_path: string, compiler_arguments: string[]): Promise<void> {
    const worker = new Worker(clangdWorkerUrl, {
      type: 'module',
      name: 'Clangd Worker',
    });
    const runtimeListener = (e: MessageEvent) => {
      switch (e.data.type) {
        case "change-compiler-arguments-done":
          console.log("change-compiler-arguments-done", e.data.id);
          this.onUpdate(e.data);
          break;
        case "error":
          this.onError(e.data.error);
          break;
      }
    };
    const promise = new Promise<void>((resolve, reject) => {
      const readyListener = (e: MessageEvent) => {
        switch (e.data?.type) {
          case "ready":
            this.onReady(worker);
            this.worker = worker;
            worker.removeEventListener("message", readyListener);
            worker.addEventListener("message", runtimeListener);
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

      worker.postMessage({ type: "init", workspace_path: workspace_path, compiler_arguments: compiler_arguments });
    });
    return promise;
  }

  change_compiler_arguments(workspace_path: string, compiler_arguments: string[]) {
    this.worker?.postMessage({ type: "change-compiler-arguments", workspace_path, compiler_arguments, id: this.#requestId++ });
  }
}
