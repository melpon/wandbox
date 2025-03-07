// 以下のソースの transport を vscode-jsonrpc の MessageConnection に変更したり
// Lint エラー修正したりいろいろ改造したやつ
//
// https://github.com/FurqanSoftware/codemirror-languageserver/blob/0e3d9343c1ffb073ea9c035dc6bc23999d9478fa/src/index.ts
//
// 元のライセンスは以下の通り。
//
// Copyright (c) 2021, Mahmud Ridwan
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// * Neither the name of the library nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import { autocompletion, insertCompletionText } from "@codemirror/autocomplete";
import { setDiagnostics } from "@codemirror/lint";
import { Facet, StateEffect, StateField } from "@codemirror/state";
import { EditorView, hoverTooltip, showTooltip, Tooltip, ViewPlugin } from "@codemirror/view";
import {
  CompletionItemKind,
  CompletionTriggerKind,
  DiagnosticSeverity,
  PublishDiagnosticsParams,
  SignatureHelpTriggerKind,
} from "vscode-languageserver-protocol";
import type * as LSP from "vscode-languageserver-protocol";

import type {
  Completion,
  CompletionContext,
  CompletionResult,
} from "@codemirror/autocomplete";
import { Extension, Text } from "@codemirror/state";
import type { PluginValue, ViewUpdate } from "@codemirror/view";
import * as marked from "marked";
import { MessageConnection } from 'vscode-jsonrpc/browser';

const timeout = 10000;
const changesDelay = 500;

const CompletionItemKindMap = Object.fromEntries(
  Object.entries(CompletionItemKind).map(([key, value]) => [value, key]),
) as Record<CompletionItemKind, string>;

const useLast = (values: readonly any[]) => values.reduce((_, v) => v, "");

const client = Facet.define<LanguageServerClient, LanguageServerClient>({ combine: useLast });
const documentUri = Facet.define<string, string>({ combine: useLast });
const languageId = Facet.define<string, string>({ combine: useLast });

// https://microsoft.github.io/language-server-protocol/specifications/specification-current/

// Client to server then server to client
interface LSPRequestMap {
  initialize: [LSP.InitializeParams, LSP.InitializeResult];
  "textDocument/hover": [LSP.HoverParams, LSP.Hover];
  "textDocument/completion": [
    LSP.CompletionParams,
    LSP.CompletionItem[] | LSP.CompletionList | null
  ];
  "textDocument/signatureHelp": [LSP.SignatureHelpParams, LSP.SignatureHelp],
}

// Client to server
interface LSPNotifyMap {
  initialized: LSP.InitializedParams;
  "textDocument/didChange": LSP.DidChangeTextDocumentParams;
  "textDocument/didOpen": LSP.DidOpenTextDocumentParams;
  "workspace/didChangeConfiguration": LSP.DidChangeConfigurationParams;
}

// Server to client
interface LSPEventMap {
  "textDocument/publishDiagnostics": LSP.PublishDiagnosticsParams;
}

type Notification = {
  [key in keyof LSPEventMap]: {
    jsonrpc: "2.0";
    id?: null | undefined;
    method: key;
    params: LSPEventMap[key];
  };
}[keyof LSPEventMap];

interface LanguageServerClientOptions {
  rootUri: string;
  workspaceFolders: LSP.WorkspaceFolder[] | null;
  connection: MessageConnection;
  autoClose?: boolean;
}

export class LanguageServerClient {
  public ready: boolean = false;
  public capabilities: LSP.ServerCapabilities<any> | null = null;

  public initializePromise: Promise<void>;
  private rootUri: string;
  private workspaceFolders: LSP.WorkspaceFolder[] | null;
  private autoClose?: boolean;

  private connection: MessageConnection;

  private plugins: LanguageServerPlugin[];

  constructor(options: LanguageServerClientOptions) {
    this.rootUri = options.rootUri;
    this.workspaceFolders = options.workspaceFolders;
    this.autoClose = options.autoClose;
    this.plugins = [];
    this.connection = options.connection;

    this.connection.onNotification((method, params) => {
      this.processNotification(method, params);
    });
    this.connection.listen();

    this.initializePromise = this.initialize();
  }

  protected getInitializationOptions(): LSP.InitializeParams["initializationOptions"] {
    return {
      capabilities: {
        textDocument: {
          hover: {
            dynamicRegistration: true,
            contentFormat: ["markdown", "plaintext"],
          },
          moniker: {},
          synchronization: {
            dynamicRegistration: true,
            willSave: false,
            didSave: false,
            willSaveWaitUntil: false,
          },
          completion: {
            dynamicRegistration: true,
            completionItem: {
              snippetSupport: false,
              commitCharactersSupport: true,
              documentationFormat: ["markdown", "plaintext"],
              deprecatedSupport: false,
              preselectSupport: false,
            },
            contextSupport: false,
          },
          signatureHelp: {
            dynamicRegistration: true,
            signatureInformation: {
              documentationFormat: ["markdown", "plaintext"],
            },
            parameterInformation: {
              labelOffsetSupport: true,
            },
            activeParameterSupport: true,
          },
          declaration: {
            dynamicRegistration: true,
            linkSupport: true,
          },
          definition: {
            dynamicRegistration: true,
            linkSupport: true,
          },
          typeDefinition: {
            dynamicRegistration: true,
            linkSupport: true,
          },
          implementation: {
            dynamicRegistration: true,
            linkSupport: true,
          },
        },
        workspace: {
          didChangeConfiguration: {
            dynamicRegistration: true,
          },
        },
      },
      initializationOptions: null,
      processId: null,
      rootUri: this.rootUri,
      workspaceFolders: this.workspaceFolders,
    }
  }

  public async initialize() {
    const { capabilities } = await this.request(
      "initialize",
      this.getInitializationOptions(),
      timeout * 3,
    );
    this.capabilities = capabilities;
    console.log("ServerCapabilities", this.capabilities);
    this.notify("initialized", {});
    this.ready = true;
  }

  public close() {
    this.connection.dispose();
  }

  public textDocumentDidOpen(params: LSP.DidOpenTextDocumentParams) {
    return this.notify("textDocument/didOpen", params);
  }

  public textDocumentDidChange(params: LSP.DidChangeTextDocumentParams) {
    return this.notify("textDocument/didChange", params);
  }

  public async textDocumentHover(params: LSP.HoverParams) {
    return await this.request("textDocument/hover", params, timeout);
  }

  public async textDocumentCompletion(params: LSP.CompletionParams) {
    return await this.request("textDocument/completion", params, timeout);
  }

  public async textDocumentSignatureHelp(params: LSP.SignatureHelpParams) {
    return await this.request("textDocument/signatureHelp", params, timeout);
  }

  public async workspaceDidChangeConfiguration(params: LSP.DidChangeConfigurationParams) {
    return await this.notify("workspace/didChangeConfiguration", params);
  }

  public attachPlugin(plugin: LanguageServerPlugin) {
    this.plugins.push(plugin);
  }

  public detachPlugin(plugin: LanguageServerPlugin) {
    const i = this.plugins.indexOf(plugin);
    if (i === -1) { return; }
    this.plugins.splice(i, 1);
    if (this.autoClose) { this.close(); }
  }

  protected request<K extends keyof LSPRequestMap>(
    method: K,
    params: LSPRequestMap[K][0],
    timeout: number,
  ): Promise<LSPRequestMap[K][1]> {
    console.log("request", method, params);
    return this.connection.sendRequest(method, params);
  }

  protected notify<K extends keyof LSPNotifyMap>(
    method: K,
    params: LSPNotifyMap[K],
  ): Promise<void> {
    console.log("notify", method, params);
    return this.connection.sendNotification(method, params);
  }

  protected processNotification(method: string, params: any) {
    console.log("processNotification", method, params);
    for (const plugin of this.plugins) {
      plugin.processNotification(method, params);
    }
  }

  public update() {
    for (const plugin of this.plugins) {
      plugin.requestUpdate();
    }
  }
}

class LanguageServerPlugin implements PluginValue {
  public client: LanguageServerClient;

  private documentUri: string;
  private languageId: string;
  private documentVersion: number;

  private changesTimeout: number;

  public signatureHelpOpened: boolean = false;
  private activeSignatureHelp: LSP.SignatureHelp | null = null;
  private activeSignature: number | null = null;

  private lastSentDocumentText: string = "";

  constructor(private view: EditorView, private allowHTMLContent: boolean) {
    this.client = this.view.state.facet(client);
    this.documentUri = this.view.state.facet(documentUri);
    this.languageId = this.view.state.facet(languageId);
    this.documentVersion = 0;
    this.changesTimeout = 0;

    this.client.attachPlugin(this);

    this.initialize({
      documentText: this.view.state.doc.toString(),
    });
  }

  public update({ docChanged }: ViewUpdate) {
    if (!docChanged) { return; }
    if (this.changesTimeout) { clearTimeout(this.changesTimeout); }
    this.changesTimeout = self.setTimeout(() => {
      this.sendChange({
        documentText: this.view.state.doc.toString(),
      });
    }, changesDelay);
  }

  public destroy() {
    this.client.detachPlugin(this);
  }

  public async initialize({ documentText }: { documentText: string }) {
    if (this.client.initializePromise) {
      await this.client.initializePromise;
    }
    this.lastSentDocumentText = documentText;
    this.client.textDocumentDidOpen({
      textDocument: {
        uri: this.documentUri,
        languageId: this.languageId,
        text: documentText,
        version: this.documentVersion,
      },
    });
  }

  public async sendChange({ documentText }: { documentText: string }) {
    if (!this.client.ready) { return; }
    if (this.lastSentDocumentText === documentText) { return; }
    try {
      this.lastSentDocumentText = documentText;
      await this.client.textDocumentDidChange({
        textDocument: {
          uri: this.documentUri,
          version: this.documentVersion++,
        },
        contentChanges: [{ text: documentText }],
      });
    } catch (e) {
      console.error(e);
    }
  }

  public requestUpdate() {
    this.sendChange({ documentText: this.view.state.doc.toString() });
  }
  public requestDiagnostics(view: EditorView) {
    this.sendChange({ documentText: view.state.doc.toString() });
  }

  public async requestHoverTooltip(
    view: EditorView,
    { line, character }: { line: number; character: number },
  ): Promise<Tooltip | null> {
    if (!this.client.ready || !this.client.capabilities!.hoverProvider) { return null; }

    this.sendChange({ documentText: view.state.doc.toString() });
    const result = await this.client.textDocumentHover({
      textDocument: { uri: this.documentUri },
      position: { line, character },
    });
    if (!result) { return null; }
    const { contents, range } = result;
    let pos = posToOffset(view.state.doc, { line, character })!;
    let end: number;
    if (range) {
      pos = posToOffset(view.state.doc, range.start)!;
      end = posToOffset(view.state.doc, range.end);
    }
    if (pos === null) { return null; }
    const dom = document.createElement("div");
    dom.classList.add("documentation");
    if (this.allowHTMLContent) {
      dom.innerHTML = formatContents(contents);
    } else {
      dom.textContent = formatContents(contents);
    }
    return {
      pos,
      end,
      create: (view) => ({ dom }),
      above: true,
    };
  }

  public async requestCompletion(
    context: CompletionContext,
    { line, character }: { line: number; character: number },
    {
      triggerKind,
      triggerCharacter,
    }: {
      triggerKind: CompletionTriggerKind;
      triggerCharacter: string | undefined;
    },
  ): Promise<CompletionResult | null> {
    if (!this.client.ready || !this.client.capabilities!.completionProvider) { return null; }
    this.sendChange({
      documentText: context.state.doc.toString(),
    });

    const result = await this.client.textDocumentCompletion({
      textDocument: { uri: this.documentUri },
      position: { line, character },
      context: {
        triggerKind,
        triggerCharacter,
      },
    });

    if (!result) { return null; }

    let items = "items" in result ? result.items : result;

    const [span, match] = prefixMatch(items);
    const token = context.matchBefore(match);
    let { pos } = context;

    if (token) {
      pos = token.from;
      const word = token.text.toLowerCase();
      if (/^\w+$/.test(word)) {
        items = items
          .filter(({ label, filterText }) => {
            const text = filterText ?? label;
            return text.toLowerCase().startsWith(word);
          })
          .sort((a, b) => {
            const aText = a.sortText ?? a.label;
            const bText = b.sortText ?? b.label;
            if (aText < bText) return -1;
            if (aText > bText) return 1;
            return 0;
          });
      }
    }

    const options = items.map(
      ({
        detail,
        label,
        kind,
        insertText,
        textEdit,
        documentation,
        additionalTextEdits,
      }) => {

        const completion: Completion = {
          label: insertText,
          displayLabel: label,
          detail,
          apply(view: EditorView, completion: Completion, from: number, to: number) {
            if (isLSPTextEdit(textEdit)) {
              view.dispatch(
                insertCompletionText(
                  view.state,
                  textEdit.newText,
                  posToOffset(view.state.doc, textEdit.range.start),
                  posToOffset(view.state.doc, textEdit.range.end),
                ),
              );
            } else {
              view.dispatch(insertCompletionText(view.state, label, from, to));
            }
            if (!additionalTextEdits) {
              return;
            }
            additionalTextEdits
              .sort(({ range: { end: a } }, { range: { end: b } }) => {
                if (posToOffset(view.state.doc, a) < posToOffset(view.state.doc, b)) {
                  return 1;
                } else if (posToOffset(view.state.doc, a) > posToOffset(view.state.doc, b)) {
                  return -1;
                }
                return 0;
              })
              .forEach((textEdit) => {
                view.dispatch(view.state.update({
                  changes: {
                    from: posToOffset(view.state.doc, textEdit.range.start),
                    to: posToOffset(view.state.doc, textEdit.range.end),
                    insert: textEdit.newText,
                  },
                }));
              });
          },
          type: kind && CompletionItemKindMap[kind].toLowerCase(),
        };
        if (documentation) {
          const contents = formatContents(documentation);
          completion.info = () => {
            const dom = document.createElement("div");
            dom.classList.add("documentation");
            dom.innerHTML = contents;
            return dom;
          };
        }
        return completion;
      },
    );

    return {
      from: pos,
      options,
      getMatch: (completion, matched) => {
        // displayLabel の何文字目から何文字目まで label なのかを調べる
        const { displayLabel, label } = completion;
        const start = displayLabel?.indexOf(label);
        if (start === undefined || start === -1) {
          return [];
        }
        const end = start + label.length;
        return [start, end];
      },
    };
  }

  createSignatureHelpTooltip(pos: number, signatureHelp: LSP.SignatureHelp): Tooltip {
    return {
      pos: pos,
      create: (view) => {
        // e   <div class="cm-signatureHelp">
        // e1    <div class="cm-signatureHelp-selection">左要素
        // e1a     <div>上矢印</div>
        // e1b     <div>1/3</div>
        // e1c     <div>上矢印</div>
        //       </div>
        // e2    <div /> ← セパレーター
        // e3    <div>右要素
        // e3a     <div class="cm-signatureHelp-signature">ここにシグネチャの内容</div>
        // e3b     <hr />
        // e3c     <div>ここにパラメータのドキュメント</div>
        // e3d     <hr />
        // e3e     <div>ここにシグネチャのドキュメント</div>
        //       </div>
        //     </div>
        const e = document.createElement("div");
        const e1 = document.createElement("div");
        const e1a = document.createElement("div");
        const e1b = document.createElement("div");
        const e1c = document.createElement("div");
        const e2 = document.createElement("div");
        const e3 = document.createElement("div");
        const e3a = document.createElement("div");
        const e3b = document.createElement("hr");
        const e3c = document.createElement("div");
        const e3d = document.createElement("hr");
        const e3e = document.createElement("div");
        e.appendChild(e1);
        e.appendChild(e2);
        e.appendChild(e3);
        e1.appendChild(e1a);
        e1.appendChild(e1b);
        e1.appendChild(e1c);
        e3.appendChild(e3a);

        e.classList.add("cm-signatureHelp");
        e1.classList.add("cm-signatureHelp-selection");
        e3a.classList.add("cm-signatureHelp-signature");

        // CSS は全部ここでやってしまう
        e.classList.add("flex", "flex-row", "align-items-end");
        e.style.cssText = "max-width: 500px;";
        e1.classList.add("flex-column", "align-items-center", "px-2px", "flex-grow-1");
        e1a.classList.add("flex", "justify-content-center");
        // bi bi-chevron-up
        e1a.innerHTML = String.raw`
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chevron-up" viewBox="0 0 16 16">
              <path fill-rule="evenodd" d="M7.646 4.646a.5.5 0 0 1 .708 0l6 6a.5.5 0 0 1-.708.708L8 5.707l-5.646 5.647a.5.5 0 0 1-.708-.708z"/>
            </svg>`
        e1b.style.cssText = "margin-top: -4px; margin-bottom: -4px;";
        e1c.classList.add("flex", "justify-content-center");
        // bi bi-chevron-down
        e1c.innerHTML = String.raw`
            <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chevron-down" viewBox="0 0 16 16">
              <path fill-rule="evenodd" d="M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708"/>
            </svg>`
        e2.classList.add("border-left", "h-100");
        e3.classList.add("flex-column", "px-2px", "align-self-start", "flex-grow-1");
        e3.style.cssText = "border-left: 1px solid #bbb";
        e3a.classList.add("py-4px", "lh-sm");
        e3a.style.cssText = "font-family: monospace;";
        e3b.classList.add("m-0");
        e3c.classList.add("py-4px", "lh-sm", "text-break");
        e3d.classList.add("m-0");
        e3e.classList.add("py-4px", "lh-sm", "text-break");

        e1a.onclick = () => {
          if (this.activeSignatureHelp === null) { return; }
          const signatures = signatureHelp.signatures
          this.activeSignature = ((this.activeSignature || 0) + signatures.length - 1) % signatures.length;
          view.dispatch({
            effects: signatureHelpOpen.of({ tooltip: this.createSignatureHelpTooltip(pos, this.activeSignatureHelp) }),
          })
          view.focus();
        };
        e1c.onclick = () => {
          if (this.activeSignatureHelp === null) { return; }
          const signatures = signatureHelp.signatures
          this.activeSignature = ((this.activeSignature || 0) + 1) % signatures.length;
          view.dispatch({
            effects: signatureHelpOpen.of({ tooltip: this.createSignatureHelpTooltip(pos, this.activeSignatureHelp) }),
          })
          view.focus();
        };

        const signature = signatureHelp.signatures[this.activeSignature || 0];
        const parameter = signature.parameters === undefined || signature.parameters.length == 0 ? null : signature.parameters[signatureHelp.activeParameter || 0];
        e1b.textContent = ((this.activeSignature || 0) + 1) + "/" + signatureHelp.signatures.length;
        const paramLabel = parameter === null ? null : typeof parameter.label === "string" ? parameter.label : signature.label.substring(parameter.label[0], parameter.label[1]);
        const index = paramLabel === null ? -1 : signature.label.indexOf(paramLabel);
        if (index === -1) {
          e3a.textContent = signature.label;
        } else {
          // paramLabel の部分を強調表示する
          const start = signature.label.substring(0, index);
          const end = signature.label.substring(index + paramLabel.length);
          if (start.length > 0) {
            const e3ax = document.createElement("span");
            e3ax.textContent = start;
            e3a.appendChild(e3ax);
          }
          {
            const e3ax = document.createElement("strong");
            e3ax.textContent = paramLabel;
            e3a.appendChild(e3ax);
          }
          if (end.length > 0) {
            const e3ax = document.createElement("span");
            e3ax.textContent = end;
            e3a.appendChild(e3ax);
          }
        }
        if (parameter !== null && parameter.documentation !== undefined) {
          e3c.innerHTML = formatContents(parameter.documentation);
          e3.appendChild(e3b);
          e3.appendChild(e3c);
        }
        if (signature.documentation !== undefined) {
          e3e.innerHTML = formatContents(signature.documentation);
          e3.appendChild(e3d);
          e3.appendChild(e3e);
        }
        return { dom: e };
      },
      above: true,
    }
  }

  public async requestSignatureHelp(
    view: EditorView,
    triggerCharacter: string | null,
    isRetrigger: boolean,
  ): Promise<{ tooltip: Tooltip | null } | null> {
    if (!this.client.ready || !this.client.capabilities!.signatureHelpProvider) { return null; }
    this.sendChange({
      documentText: view.state.doc.toString(),
    });
    const offset = view.state.selection.main.from;

    const result = await this.client.textDocumentSignatureHelp({
      textDocument: { uri: this.documentUri },
      position: offsetToPos(view.state.doc, offset),
      context: {
        triggerKind: triggerCharacter === null ? SignatureHelpTriggerKind.ContentChange : SignatureHelpTriggerKind.TriggerCharacter,
        triggerCharacter: triggerCharacter || undefined,
        isRetrigger,
        activeSignatureHelp: this.activeSignatureHelp || undefined,
      },
    });
    this.activeSignatureHelp = result;
    if (result.signatures.length === 0) {
      return { tooltip: null };
    }
    this.signatureHelpOpened = true;
    this.activeSignature = result.activeSignature || 0;

    return {
      tooltip: this.createSignatureHelpTooltip(offset, result),
    }
  }

  public processNotification(method: string, params: any) {
    try {
      switch (method) {
        case "textDocument/publishDiagnostics":
          this.processDiagnostics(params);
      }
    } catch (error) {
      console.log(error);
    }
  }

  public processDiagnostics(params: PublishDiagnosticsParams) {
    console.log(params.uri, this.documentUri);
    if (params.uri !== this.documentUri) { return; }

    const diagnostics = params.diagnostics
      .map(({ range, message, severity }) => ({
        from: posToOffset(this.view.state.doc, range.start)!,
        to: posToOffset(this.view.state.doc, range.end)!,
        severity: ({
          [DiagnosticSeverity.Error]: "error",
          [DiagnosticSeverity.Warning]: "warning",
          [DiagnosticSeverity.Information]: "info",
          [DiagnosticSeverity.Hint]: "info",
        } as const)[severity!],
        message,
      }))
      .filter(({ from, to }) => from !== null && to !== null && from !== undefined && to !== undefined)
      .sort((a, b) => {
        switch (true) {
          case a.from < b.from:
            return -1;
          case a.from > b.from:
            return 1;
        }
        return 0;
      });

    this.view.dispatch(setDiagnostics(this.view.state, diagnostics));
  }
}

type SignatureHelpConfig = {
  keydown: (key: string, view: EditorView) => void;
  update: (view: EditorView) => void;
}
const signatureHelpConfig = Facet.define<SignatureHelpConfig, SignatureHelpConfig>({
  combine: (values) => values.length === 0 ? { keydown: () => { }, update: () => { } } : values[0]
});
type SignatureHelpState = {
  tooltip: Tooltip | null,
}
const signatureHelpOpen = StateEffect.define<{ tooltip: Tooltip }>();
const signatureHelpClose = StateEffect.define();

const signatureHelpField = StateField.define<SignatureHelpState>({
  create() {
    return {
      tooltip: null,
    };
  },
  update(value, tr) {
    for (const effect of tr.effects) {
      if (effect.is(signatureHelpOpen)) {
        return {
          tooltip: effect.value.tooltip,
        };
      }
      if (effect.is(signatureHelpClose)) {
        return {
          tooltip: null,
        };
      }
    }
    return value;
  },
  provide: f => showTooltip.compute([f], state => state.field(f).tooltip),
});

const signatureHelpUpdateListener = EditorView.updateListener.of((update) => {
  const config = update.view.state.facet(signatureHelpConfig);
  const field = update.view.state.field(signatureHelpField, false);
  if (field === undefined) {
    return false;
  }
  if (!update.selectionSet && !update.docChanged) {
    return false;
  }

  let key: string | null = null;
  for (const tr of update.transactions) {
    const typing = tr.isUserEvent("input.type");
    if (typing) {
      const pos = tr.newSelection.main.from;
      key = tr.newDoc.sliceString(pos - 1, pos);
      break;
    }
  }
  if (key !== null) {
    config.keydown(key, update.view);
  }

  // signatureHelp が出てる間はカーソルの移動や変更などでも更新する
  if (key === null && field.tooltip !== null) {
    config.update(update.view);
  }

});
//export const signatureHelpCharacters = EditorView.domEventHandlers({
//  keydown(event, view) {
//    const config = view.state.facet(signatureHelpConfig);
//    const field = view.state.field(signatureHelpField, false);
//    if (field === undefined) {
//      return false;
//    }
//    return config.keydown(event, view)
//  }
//});

function createSignatureHelp(config: SignatureHelpConfig): Extension {
  return [
    signatureHelpConfig.of(config),
    signatureHelpField,
    signatureHelpUpdateListener,
  ]
}

interface LanguageServerOptions {
  client: LanguageServerClient;
  documentUri: string;
  languageId: string;
  allowHTMLContent: boolean;
}

export function createLanguageServerExtension(options: LanguageServerOptions) {
  let plugin: LanguageServerPlugin | null = null;

  return [
    client.of(options.client),
    documentUri.of(options.documentUri),
    languageId.of(options.languageId),
    ViewPlugin.define((view) => (plugin = new LanguageServerPlugin(view, options.allowHTMLContent))),
    hoverTooltip(
      (view, pos) =>
        plugin?.requestHoverTooltip(
          view,
          offsetToPos(view.state.doc, pos),
        ) ?? null,
    ),
    autocompletion({
      override: [
        async (context) => {
          if (plugin == null) { return null; }

          const { state, pos, explicit } = context;
          const line = state.doc.lineAt(pos);
          let trigKind: CompletionTriggerKind =
            CompletionTriggerKind.Invoked;
          let trigChar: string | undefined;
          if (
            !explicit &&
            plugin.client.capabilities?.completionProvider?.triggerCharacters?.includes(
              line.text[pos - line.from - 1],
            )
          ) {
            trigKind = CompletionTriggerKind.TriggerCharacter;
            trigChar = line.text[pos - line.from - 1];
          }
          if (
            trigKind === CompletionTriggerKind.Invoked &&
            !context.matchBefore(/\w+$/)
          ) {
            return null;
          }
          return await plugin.requestCompletion(
            context,
            offsetToPos(state.doc, pos),
            {
              triggerCharacter: trigChar,
              triggerKind: trigKind,
            },
          );
        },
      ],
    }),
    createSignatureHelp({
      keydown: (key, view) => {
        if (plugin == null) { return; }

        const triggerCharacters = plugin.client.capabilities?.signatureHelpProvider?.triggerCharacters || [];
        const retriggerCharacters = plugin.client.capabilities?.signatureHelpProvider?.retriggerCharacters || [];
        const triggerIndex = triggerCharacters.indexOf(key);
        const retriggerIndex = retriggerCharacters.indexOf(key);
        const isRetrigger = plugin.signatureHelpOpened && retriggerIndex !== -1;
        if (triggerIndex === -1 && !isRetrigger) {
          return;
        }
        plugin.requestSignatureHelp(view, key, isRetrigger).then(r => {
          if (r === null) {
            return;
          }
          view.dispatch({
            effects: r.tooltip === null ? signatureHelpClose.of(null) : signatureHelpOpen.of({ tooltip: r.tooltip }),
          })
        });
      },
      update: (view) => {
        if (plugin == null) { return; }
        plugin.requestSignatureHelp(view, null, false).then(r => {
          if (r === null) {
            return;
          }
          view.dispatch({
            effects: r.tooltip === null ? signatureHelpClose.of(null) : signatureHelpOpen.of({ tooltip: r.tooltip }),
          })
        });
      }
    }),
  ];
}

function posToOffset(doc: Text, pos: { line: number; character: number }) {
  if (pos.line >= doc.lines) { return; }
  const offset = doc.line(pos.line + 1).from + pos.character;
  if (offset > doc.length) { return; }
  return offset;
}

function offsetToPos(doc: Text, offset: number) {
  const line = doc.lineAt(offset);
  return {
    character: offset - line.from,
    line: line.number - 1,
  };
}

function formatContents(
  contents: LSP.MarkupContent | LSP.MarkedString | LSP.MarkedString[],
): string {
  if (isLSPMarkupContent(contents)) {
    let value = contents.value;
    if (contents.kind === "markdown") {
      value = marked.parse(value, { async: false });
    }
    return value;
  } else if (Array.isArray(contents)) {
    return contents.map((c) => formatContents(c) + "\n\n").join("");
  } else if (typeof contents === "string") {
    return contents;
  }
}

function toSet(chars: Set<string>) {
  let preamble = "";
  let flat = Array.from(chars).join("");
  const words = /\w/.test(flat);
  if (words) {
    preamble += "\\w";
    flat = flat.replace(/\w/g, "");
  }
  return `[${preamble}${flat.replace(/[^\w\s]/g, "\\$&")}]`;
}

function prefixMatch(items: LSP.CompletionItem[]) {
  const first = new Set<string>();
  const rest = new Set<string>();

  for (const item of items) {
    const [initial, ...restStr] = item.textEdit?.newText || item.label;
    first.add(initial);
    for (const char of restStr) {
      rest.add(char);
    }
  }

  const source = toSet(first) + toSet(rest) + "*$";
  return [new RegExp("^" + source), new RegExp(source)];
}

function isLSPTextEdit(textEdit?: LSP.TextEdit | LSP.InsertReplaceEdit): textEdit is LSP.TextEdit {
  return (textEdit as LSP.TextEdit)?.range !== undefined;
}

function isLSPMarkupContent(
  contents: LSP.MarkupContent | LSP.MarkedString | LSP.MarkedString[],
): contents is LSP.MarkupContent {
  return (contents as LSP.MarkupContent).kind !== undefined;
}
