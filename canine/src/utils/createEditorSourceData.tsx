import { PermlinkCode } from "~/hooks/permlink";
import { EditorSourceData } from "~/contexts/EditorContext";

// PermlinkCode から EditorSourceData に変換する
export function createEditorSourceData(
  code: string,
  codes: PermlinkCode[]
): EditorSourceData[] {
  return [
    {
      filename: null as string | null,
      text: code
    }
  ].concat(
    ...codes.map(
      (code): EditorSourceData => ({
        filename: code.file,
        text: code.code
      })
    )
  );
}
