import React from "react";

import {
  CompilerList,
  SelectSwitchOption,
  CompilerInfo,
} from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { useCompilerContext } from "~/contexts/CompilerContext";
import { useEditorContext } from "~/contexts/EditorContext";
import { useResultContext } from "~/contexts/ResultContext";
import { SingleSwitch, SelectSwitch } from "~/hooks/compilerList";
import { ChooseLanguage } from "./Compiler/ChooseLanguage";
import { ChooseCompiler } from "./Compiler/ChooseCompiler";
import { CompilerOption } from "./Compiler/CompilerOption";
import { RawCompilerOption } from "./Compiler/RawCompilerOption";
import { useSidebarContext } from "~/contexts/SidebarContext";

interface CompilerProps {
  //editor: EditorState,
  //compiler: CompilerState,
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

// カンマ区切りのコンパイラオプションの文字列から、
// {[name: string]: string | boolean} のコンパイラオプション情報に変換する
function optionsToSwitch(
  optionsStr: string,
  compilerInfo: CompilerInfo
): { [name: string]: string | boolean } {
  const options = optionsStr.split(",");
  const switches: { [name: string]: string | boolean } = {};
  compilerInfo.switches.forEach((sw): void => {
    if (sw.type === "single") {
      const ssw = sw.switch as SingleSwitch;
      // checkbox
      const checked = options.findIndex((x): boolean => x === ssw.name) !== -1;
      switches[ssw.name] = checked;
    } else if (sw.type === "select") {
      const ssw = sw.switch as SelectSwitch;
      // select
      const value = ((): SelectSwitchOption => {
        // ssw.options の中から options に含まれるオプションを探す。
        // 多分複数一致することは無いはずだし、複数あってもどうしようも無いので
        // 最初に一致したものを返す。
        for (const opt of ssw.options) {
          for (const target of options) {
            if (opt.name === target) {
              return opt;
            }
          }
        }

        // ここに来ることは無いはず
        throw "おかしい";
      })();
      switches[ssw.name] = value.name;
    } else {
      throw "error";
    }
  });
  return switches;
}

const Compiler: React.FC<CompilerProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const editorContext = useEditorContext();
  const compilerContext = useCompilerContext();
  const resultContext = useResultContext();
  const sidebarContext = useSidebarContext();
  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
  } = compilerContext;
  const onSelectLanguage = React.useCallback((language): void => {
    compilerContext.setCurrentLanguage(language);
    compilerContext.setCurrentCompilerName("");
  }, []);
  const onDeselectLanguage = React.useCallback((): void => {
    compilerContext.setCurrentLanguage("");
    compilerContext.setCurrentCompilerName("");
  }, []);
  const onSelectCompiler = React.useCallback((compiler: CompilerInfo): void => {
    compilerContext.setCurrentCompilerName(compiler.name);
  }, []);
  const onDeselectCompiler = React.useCallback((): void => {
    compilerContext.setCurrentCompilerName("");
  }, []);
  const onChangeChecked = React.useCallback(
    (switchName: string, checked: boolean): void => {
      // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
      compilerContext.setCurrentSwitches((opts) => ({
        ...opts,
        [switchName]: checked,
      }));
    },
    []
  );
  const onChangeSelected = React.useCallback(
    (switchName: string, selected: string): void => {
      // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
      compilerContext.setCurrentSwitches((opts) => ({
        ...opts,
        [switchName]: selected,
      }));
    },
    []
  );
  const onChangeCompilerOptionRaw = React.useCallback(
    (cm: unknown, data: unknown, value: string): void => {
      compilerContext.setCompilerOptionRaw(value);
    },
    []
  );
  const onChangeRuntimeOptionRaw = React.useCallback(
    (cm: unknown, data: unknown, value: string): void => {
      compilerContext.setRuntimeOptionRaw(value);
    },
    []
  );
  const doCompile = useCompile(
    editorContext,
    compilerContext,
    sidebarContext,
    compilerList,
    resultContext
  );
  const onCtrlEnter = React.useCallback((): void => {
    doCompile();
  }, [doCompile]);

  const language =
    permlinkData === null
      ? currentLanguage === ""
        ? null
        : currentLanguage
      : permlinkData.parameter.compilerInfo.language;
  const languages = Object.keys(compilerList.languages).sort();

  const compilerInfo = React.useMemo((): CompilerInfo | null => {
    if (permlinkData !== null) {
      return permlinkData.parameter.compilerInfo;
    }

    if (currentCompilerName === "") {
      return null;
    }
    const info = compilerList.compilers.find(
      (info): boolean => info.name === currentCompilerName
    );
    if (info === undefined) {
      return null;
    }
    return info;
  }, [permlinkData, compilerList, currentCompilerName]);

  const compilerInfos: CompilerInfo[] =
    permlinkData === null ? compilerList.languages[currentLanguage] : [];

  const switches =
    permlinkData === null
      ? currentSwitches
      : optionsToSwitch(
          permlinkData.parameter.options,
          permlinkData.parameter.compilerInfo
        );

  const readOnly = permlinkData !== null;

  return (
    <div className="d-flex flex-column gap-16px" style={{ width: 280 }}>
      {/* choose language */}
      <ChooseLanguage
        language={language}
        languages={languages}
        readOnly={readOnly}
        onSelectLanguage={onSelectLanguage}
        onDeselectLanguage={onDeselectLanguage}
      />

      {/* choose compiler */}
      {permlinkData === null && currentLanguage === "" ? null : (
        <ChooseCompiler
          compilerInfo={compilerInfo}
          compilerInfos={compilerInfos}
          readOnly={readOnly}
          onSelectCompiler={onSelectCompiler}
          onDeselectCompiler={onDeselectCompiler}
        />
      )}

      {/* compiler options */}
      {compilerInfo === null ? null : (
        <CompilerOption
          switches={switches}
          compilerInfo={compilerInfo}
          readOnly={readOnly}
          onChangeChecked={onChangeChecked}
          onChangeSelected={onChangeSelected}
        />
      )}
      {/* raw compiler options */}
      {compilerInfo === null ? null : (
        <RawCompilerOption
          compilerOptionRaw={
            permlinkData === null
              ? compilerInfo.compilerOptionRaw
                ? compilerOptionRaw
                : null
              : permlinkData.parameter.compilerInfo.compilerOptionRaw
              ? permlinkData.parameter.compilerOptionRaw
              : null
          }
          runtimeOptionRaw={
            permlinkData === null
              ? compilerInfo.runtimeOptionRaw || runtimeOptionRaw !== ""
                ? runtimeOptionRaw
                : null
              : permlinkData.parameter.compilerInfo.runtimeOptionRaw ||
                permlinkData.parameter.runtimeOptionRaw !== ""
              ? permlinkData.parameter.runtimeOptionRaw
              : null
          }
          readOnly={readOnly}
          onChangeCompilerOptionRaw={onChangeCompilerOptionRaw}
          onChangeRuntimeOptionRaw={onChangeRuntimeOptionRaw}
          onCtrlEnter={onCtrlEnter}
        />
      )}
    </div>
  );
};

export { Compiler };
