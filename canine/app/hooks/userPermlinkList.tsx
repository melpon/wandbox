import { CompilerInfo, resolveCompilerInfo } from "./compilerList";
import type { AnyJson, JsonMap } from "./fetch";
import { useFetchJSON } from "./fetch";

export interface UserPermlink {
  parameter: {
    code: string;
    stdin: string;
    compiler: string;
    options: string;
    compilerOptionRaw: string;
    runtimeOptionRaw: string;
    createdAt: number;
    title: string;
    description: string;
    githubUser: string;
    isPrivate: boolean;
    compilerInfo: CompilerInfo;
  };
  permlinkId: string;
}
export interface UserPermlinkList {
  rows: UserPermlink[];
  currentPage: number;
  pageMax: number;
  rowsPerPage: number;
}

function resolveUserPermlink(json: AnyJson): UserPermlink {
  const map = json as JsonMap;
  const param = map.parameter as JsonMap;
  return {
    parameter: {
      code: param.code as string,
      stdin: param.stdin as string,
      compiler: param.compiler as string,
      options: param.options as string,
      compilerOptionRaw: param["compiler-option-raw"] as string,
      runtimeOptionRaw: param["runtime-option-raw"] as string,
      createdAt: param.created_at as number,
      title: param.title as string,
      description: param.description as string,
      githubUser: param.github_user as string,
      isPrivate: param.is_private as boolean,
      compilerInfo: resolveCompilerInfo(param["compiler-info"]),
    },
    permlinkId: map.permlink as string,
  };
}
function resolveUserPermlinkList(json: AnyJson): UserPermlinkList {
  const map = json as JsonMap;
  return {
    rows: (map.rows as AnyJson[]).map(resolveUserPermlink),
    currentPage: map.current_page as number,
    pageMax: map.page_max as number,
    rowsPerPage: map.rows_per_page as number,
  };
}

export function useUserPermlinkList(
  url: string,
  onError: (error: string) => void
): [
  UserPermlinkList | null,
  number,
  (url: string | null, opts: RequestInit) => void
] {
  const headers = {
    "Content-Type": "application/json",
  };
  return useFetchJSON<UserPermlinkList>(
    url,
    { method: "GET", headers: headers },
    resolveUserPermlinkList,
    onError
  );
}
