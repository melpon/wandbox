import { CompilerInfo } from "~/hooks/compilerList";
import { reduceCompileOptions } from "./reduceCompileOptions";

export function getCompileOptions(
  currentSwitches: { [name: string]: string | boolean },
  info: CompilerInfo
): string[] {
  return reduceCompileOptions<string[]>(
    currentSwitches,
    info,
    [],
    (sw, state): string[] => [...state, sw.name],
    (_sw, value, state): string[] => [...state, value]
  );
}
