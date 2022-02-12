import { PermlinkCode } from "~/hooks/permlink";
import { EditorSourceData } from "~/contexts/EditorContext";

// PermlinkCode から EditorSourceData に変換する
export function createEditorSourceData(
  code: string,
  codes: PermlinkCode[]
): EditorSourceData[] {
  return [
    {
      id: "wb-editor-main",
      filename: null,
      text: code,
    } as EditorSourceData,
  ].concat(
    ...codes.map(
      (code, index): EditorSourceData => ({
        id: `wb-editor-tab${index}`,
        filename: code.file,
        text: code.code,
      })
    )
  );
}
