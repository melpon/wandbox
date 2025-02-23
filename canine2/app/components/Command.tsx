import React, { useMemo } from "react";
import { useSelector } from "react-redux";

import { reduceCompileOptions } from "~/utils/reduceCompileOptions";
import { optionsToSwitch } from "~/utils/optionsToSwitch";
import type { CompilerList, CompilerInfo } from "~/hooks/compilerList";
import type { PermlinkData } from "~/hooks/permlink";
import type { AppState } from "~/store";

export interface CommandProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

function rawToOptions(raw: string): string {
  return raw.split("\n").join(" ");
}

const Command: React.FC<CommandProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const {
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
  } = useSelector(
    ({
      wandbox: {
        currentCompilerName,
        currentSwitches,
        compilerOptionRaw,
        runtimeOptionRaw,
      },
    }: AppState) => ({
      currentCompilerName,
      currentSwitches,
      compilerOptionRaw,
      runtimeOptionRaw,
    })
  );

  const command = useMemo((): string => {
    let info: CompilerInfo;
    if (permlinkData === null) {
      const infoUndef = compilerList.compilers.find(
        (c): boolean => c.name === currentCompilerName
      );
      if (infoUndef === undefined) {
        return "";
      }
      info = infoUndef;
    } else {
      info = permlinkData.parameter.compilerInfo;
    }

    const command = info.displayCompileCommand;
    const options = reduceCompileOptions<string[]>(
      permlinkData === null
        ? currentSwitches
        : optionsToSwitch(permlinkData.parameter.options, info),
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
      permlinkData === null
        ? info.compilerOptionRaw
          ? compilerOptionRaw
          : runtimeOptionRaw
        : info.compilerOptionRaw
        ? permlinkData.parameter.compilerOptionRaw
        : permlinkData.parameter.runtimeOptionRaw
    );
    return `$ ${command} ${options.join(" ")} ${rawOptions}`;
  }, [
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    compilerList,
  ]);

  return <code className="wb-command px-8px">{command}</code>;
};

export { Command };
