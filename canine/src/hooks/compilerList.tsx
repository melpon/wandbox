import React from "react";
import { useFetchJSON, AnyJson, JsonMap } from "./fetch";

export interface SingleSwitch {
  name: string;
  default: boolean;
  displayFlags: string;
  displayName: string;
}
export interface SelectSwitchOption {
  name: string;
  displayFlags: string;
  displayName: string;
}
export interface SelectSwitch {
  name: string;
  default: string;
  options: SelectSwitchOption[];
}
export interface Switch {
  type: "single" | "select";
  switch: SingleSwitch | SelectSwitch;
}
export interface CompilerInfo {
  name: string;
  version: string;
  language: string;
  displayName: string;
  templates: string[];
  compilerOptionRaw: boolean;
  runtimeOptionRaw: boolean;
  displayCompileCommand: string;
  switches: Switch[];
}

export interface CompilerList {
  compilers: CompilerInfo[];
  // CompilerInfo のリストを言語ごとにグループ化したもの
  languages: {
    [lang: string]: CompilerInfo[];
  };
}

function resolveSwitch(json: AnyJson): Switch {
  const obj = json as JsonMap;
  if (obj.type === "single") {
    return {
      type: "single",
      switch: {
        name: obj.name as string,
        default: obj.default as boolean,
        displayFlags: obj["display-flags"] as string,
        displayName: obj["display-name"] as string,
      },
    };
  } else if (obj.type === "select") {
    return {
      type: "select",
      switch: {
        name: obj.name as string,
        default: obj.default as string,
        options: (obj.options as AnyJson[]).map(
          (json): SelectSwitchOption => {
            const obj = json as JsonMap;
            return {
              name: obj.name as string,
              displayFlags: obj["display-flags"] as string,
              displayName: obj["display-name"] as string,
            };
          }
        ),
      },
    };
  } else {
    throw "error";
  }
}

export function resolveCompilerInfo(json: AnyJson): CompilerInfo {
  const obj = json as JsonMap;
  return {
    name: obj.name as string,
    version: obj.version as string,
    language: obj.language as string,
    displayName: obj["display-name"] as string,
    templates: obj.templates as string[],
    compilerOptionRaw: obj["compiler-option-raw"] as boolean,
    runtimeOptionRaw: obj["runtime-option-raw"] as boolean,
    displayCompileCommand: obj["display-compile-command"] as string,
    switches: (obj.switches as AnyJson[]).map(resolveSwitch),
  };
}

export function useCompilerList(
  url: string,
  onError: (error: string) => void
): CompilerList | null {
  const headers = {
    "Content-Type": "application/json",
  };

  const resolver = React.useCallback((json): CompilerInfo[] => {
    console.log(json);
    return (json as AnyJson[]).map(resolveCompilerInfo);
  }, []);

  const [compilerInfos, , doFetch] = useFetchJSON<CompilerInfo[]>(
    url,
    { method: "GET", headers: headers },
    resolver,
    onError
  );

  React.useEffect((): void => {
    doFetch(null, {});
  }, []);

  if (compilerInfos === null) {
    return null;
  }

  const languages: { [lang: string]: CompilerInfo[] } = {};
  for (const info of compilerInfos) {
    if (info.language in languages) {
      languages[info.language].push(info);
    } else {
      languages[info.language] = [info];
    }
  }

  return {
    compilers: compilerInfos,
    languages: languages,
  };
}
