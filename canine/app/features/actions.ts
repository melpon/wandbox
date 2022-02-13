import { EditorView } from "@codemirror/view";
import { AppDispatch } from "~/store";
import { EditorSourceData, wandboxSlice } from "./slice";

const actions = wandboxSlice.actions;

export function getSourceText(source: EditorSourceData): string {
  return source.text !== undefined
    ? source.text
    : source.view!.state.doc.toString();
}

export function addSource(
  dispatch: AppDispatch,
  sources: EditorSourceData[],
  filename: string,
  text?: string
): number {
  dispatch(actions.addSource({ filename, text }));
  return sources.length;
}

export function setView(
  dispatch: AppDispatch,
  sources: EditorSourceData[],
  tab: number,
  view: EditorView
) {
  if (sources[tab].text !== undefined) {
    view.dispatch({
      changes: {
        from: 0,
        to: view.state.doc.length,
        insert: sources[tab].text,
      },
    });
  }

  dispatch(actions.setView({ tab, view }));
}
