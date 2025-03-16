import { Message } from 'vscode-jsonrpc/browser';

const CR = 13;
const LF = 10;

export class StdoutParser {
  #state: 0 | 1 = 0;
  #token: number[] = [];
  #contentLength: number = 0;
  readonly #textDecoder = new TextDecoder("utf-8");

  onRead: (message: Message) => void = () => { };
  add(charCode: number) {
    this.#token.push(charCode & 0xff);
    if (this.#state === 0) {
      // \r\n\r\n という値が続くまで読む
      if (charCode === LF &&
        this.#token.length >= 4 &&
        this.#token[this.#token.length - 2] === CR &&
        this.#token[this.#token.length - 3] === LF &&
        this.#token[this.#token.length - 4] === CR) {
        // #token は "Content-Length: 1234\r\n\r\n"
        // みたいな文字列になっているはずなので、
        // ここから 1234 の部分を取り出す
        const header = this.#textDecoder.decode(new Uint8Array(this.#token));
        header.split("\r\n").forEach((line) => {
          if (line.startsWith("Content-Length:")) {
            this.#contentLength = parseInt(line.slice(15));
          }
        });
        if (this.#contentLength !== 0) {
          this.#state = 1;
          this.#token.length = 0;
        }
      }
    } else {
      if (this.#token.length === this.#contentLength) {
        const message = JSON.parse(this.#textDecoder.decode(new Uint8Array(this.#token)));
        this.onRead(message);
        this.#state = 0;
        this.#token.length = 0;
        this.#contentLength = 0;
      }
    }
  }
}
