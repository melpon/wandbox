import type { AnyJson, JsonMap } from "./fetch";
import { useFetchJSON } from "./fetch";

export interface TemplateCode {
  file: string;
  code: string;
}
export interface TemplateGetData {
  code: string;
  codes: TemplateCode[];
}

function resolveTemplateGetData(json: AnyJson): TemplateGetData {
  const map = json as JsonMap;
  return {
    code: map.code as string,
    codes:
      map.codes === undefined ? [] : (map.codes as unknown as TemplateCode[]),
  };
}

export function useGetTemplate(
  url: string,
  onError: (error: string) => void
): [
  TemplateGetData | null,
  number,
  (url: string | null, opts: RequestInit) => void
] {
  const headers = {
    "Content-Type": "application/json",
  };
  return useFetchJSON<TemplateGetData>(
    url,
    { method: "GET", headers: headers },
    resolveTemplateGetData,
    onError
  );
}
