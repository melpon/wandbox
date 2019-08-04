import { useFetchJSON, AnyJson, JsonMap } from "./fetch";
import { CompilerInfo, resolveCompilerInfo } from "./compilerList";
import { Result } from "~/contexts/ResultContext";

export interface PermlinkPostData {
  permlink: string;
}

function resolvePermlinkPostData(json: AnyJson): PermlinkPostData {
  const map = json as JsonMap;
  return {
    permlink: map.permlink as string
  };
}

export function usePostPermlink(
  url: string,
  onError: (error: string) => void
): [
  PermlinkPostData | null,
  number,
  ((url: string | null, opts: RequestInit) => void)
] {
  const headers = {
    "Content-Type": "application/json"
  };
  return useFetchJSON<PermlinkPostData>(
    url,
    { method: "POST", headers: headers },
    resolvePermlinkPostData,
    onError
  );
}

export interface PermlinkCode {
  file: string;
  code: string;
}
export interface PermlinkData {
  permlinkId: string;
  parameter: {
    code: string;
    codes: PermlinkCode[];
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
  results: Result[];
}

function resolvePermlinkData(permlinkId: string, json: AnyJson): PermlinkData {
  const map = json as JsonMap;
  const param = map.parameter as JsonMap;
  return {
    permlinkId: permlinkId,
    parameter: {
      code: param.code as string,
      codes:
        param.codes === undefined
          ? []
          : ((param.codes as unknown) as PermlinkCode[]),
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
      compilerInfo: resolveCompilerInfo(param["compiler-info"])
    },
    results: (param.results as unknown) as Result[]
  };
}

export function useGetPermlink(
  permlinkId: string,
  onError: (error: string) => void
): [
  PermlinkData | null,
  number,
  ((url: string | null, opts: RequestInit) => void)
] {
  const headers = {
    "Content-Type": "application/json"
  };
  return useFetchJSON<PermlinkData>(
    null,
    { method: "GET", headers: headers },
    (json): PermlinkData => resolvePermlinkData(permlinkId, json),
    onError
  );
}
