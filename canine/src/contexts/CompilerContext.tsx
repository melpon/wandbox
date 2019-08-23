import React from "react";
import { createContainer } from "unstated-next";

export interface CompilerContextState {
  currentLanguage: string;
  currentCompilerName: string;
  currentSwitches: { [name: string]: string | boolean };
  compilerOptionRaw: string;
  runtimeOptionRaw: string;
  setCurrentLanguage: React.Dispatch<React.SetStateAction<string>>;
  setCurrentCompilerName: React.Dispatch<React.SetStateAction<string>>;
  setCurrentSwitches: React.Dispatch<
    React.SetStateAction<{ [name: string]: string | boolean }>
  >;
  setCompilerOptionRaw: React.Dispatch<React.SetStateAction<string>>;
  setRuntimeOptionRaw: React.Dispatch<React.SetStateAction<string>>;
}

function useCompilerContext(): CompilerContextState {
  const [currentLanguage, setCurrentLanguage] = React.useState<string>("");
  const [currentCompilerName, setCurrentCompilerName] = React.useState<string>(
    ""
  );
  const [currentSwitches, setCurrentSwitches] = React.useState<{
    [name: string]: string | boolean;
  }>({});
  const [compilerOptionRaw, setCompilerOptionRaw] = React.useState<string>("");
  const [runtimeOptionRaw, setRuntimeOptionRaw] = React.useState<string>("");
  return {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    setCurrentLanguage,
    setCurrentCompilerName,
    setCurrentSwitches,
    setCompilerOptionRaw,
    setRuntimeOptionRaw
  };
}

const CompilerContext = createContainer(useCompilerContext);
export { CompilerContext };
