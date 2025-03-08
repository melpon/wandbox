
export class AsyncRunningQueue {
  #queue: (() => Promise<void>)[] = [];
  #running = false;

  push(task: () => Promise<void>) {
    this.#queue.push(task);
    if (!this.#running) {
      this.run();
    }
  }

  async run() {
    this.#running = true;
    while (this.#queue.length > 0) {
      await this.#queue.shift()!();
    }
    this.#running = false;
  }
}
