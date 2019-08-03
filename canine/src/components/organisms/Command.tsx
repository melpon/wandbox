import React from "react";
import { useContainer } from "unstated-next";

import { reduceCompileOptions } from "~/utils/reduceCompileOptions";
import { CompilerList } from "~/hooks/compilerList";
import { CompilerContext } from "~/contexts/CompilerContext";

export interface CommandProps {
  compilerList: CompilerList;
}

function rawToOptions(raw: string): string {
  return raw.split("\n").join(" ");
}

export const Command: React.FC<CommandProps> = (props): React.ReactElement => {
  const { compilerList } = props;
  const compiler = useContainer(CompilerContext);

  const command = React.useMemo((): string => {
    const info = compilerList.compilers.find(
      (c): boolean => c.name === compiler.currentCompilerName
    );
    if (info === undefined) {
      return "";
    }

    const command = info.displayCompileCommand;
    const options = reduceCompileOptions<string[]>(
      compiler.currentSwitches,
      info,
      [],
      (sw, state): string[] => [...state, sw.displayFlags],
      (sw, value, state): string[] => {
        const opt = sw.options.find((opt): boolean => opt.name === value);
        if (opt === undefined) {
          throw "something wrong";
        }
        return [...state, opt.displayFlags];
      }
    );
    const rawOptions = rawToOptions(
      info.compilerOptionRaw
        ? compiler.compilerOptionRaw
        : compiler.runtimeOptionRaw
    );
    return `$ ${command} ${options.join(" ")} ${rawOptions}`;
  }, [compiler, compilerList]);

  return <code>{command}</code>;
};
