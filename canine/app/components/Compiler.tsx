import React, { useCallback, useMemo } from "react";
import { useSelector } from "react-redux";

import {
  CompilerList,
  SelectSwitchOption,
  CompilerInfo,
} from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { SingleSwitch, SelectSwitch } from "~/hooks/compilerList";
import { ChooseLanguage } from "./Compiler/ChooseLanguage";
import { ChooseCompiler } from "./Compiler/ChooseCompiler";
import { CompilerOption } from "./Compiler/CompilerOption";
import { RawCompilerOption } from "./Compiler/RawCompilerOption";
import { AppState, useAppDispatch, useAppStore } from "~/store";
import { wandboxSlice } from "~/features/slice";

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

const Compiler: React.FC<CompilerProps> = (props) => {
  const { compilerList, permlinkData } = props;
  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
  } = useSelector(
    ({
      wandbox: {
        currentLanguage,
        currentCompilerName,
        currentSwitches,
        compilerOptionRaw,
        runtimeOptionRaw,
      },
    }: AppState) => ({
      currentLanguage,
      currentCompilerName,
      currentSwitches,
      compilerOptionRaw,
      runtimeOptionRaw,
    })
  );

  const actions = wandboxSlice.actions;
  const dispatch = useAppDispatch();
  const onSelectLanguage = useCallback((language): void => {
    dispatch(actions.setCurrentLanguage(language));
    // 言語を選択するとコンパイラ名を自動で設定済みにする
    // head 以外のコンパイラを設定する
    for (const ci of compilerList.languages[language]) {
      if (ci.name.indexOf("head") === -1) {
        dispatch(actions.setCurrentCompilerName(ci.name));
        break;
      }
    }
  }, []);
  const onDeselectLanguage = useCallback((): void => {
    dispatch(actions.setCurrentLanguage(""));
    dispatch(actions.setCurrentCompilerName(""));
  }, []);
  const onSelectCompiler = useCallback((compiler: CompilerInfo): void => {
    dispatch(actions.setCurrentCompilerName(compiler.name));
  }, []);
  const onDeselectCompiler = useCallback((): void => {
    dispatch(actions.setCurrentCompilerName(""));
  }, []);
  const onChangeChecked = useCallback(
    (switchName: string, checked: boolean): void => {
      dispatch(actions.setCurrentSwitch({ switchName, value: checked }));
    },
    []
  );
  const onChangeSelected = useCallback(
    (switchName: string, selected: string): void => {
      dispatch(actions.setCurrentSwitch({ switchName, value: selected }));
    },
    []
  );
  const onChangeCompilerOptionRaw = useCallback((value: string): void => {
    dispatch(actions.setCompilerOptionRaw(value));
  }, []);
  const onChangeRuntimeOptionRaw = useCallback((value: string): void => {
    dispatch(actions.setRuntimeOptionRaw(value));
  }, []);

  const language =
    permlinkData === null
      ? currentLanguage === ""
        ? null
        : currentLanguage
      : permlinkData.parameter.compilerInfo.language;
  const languages = Object.keys(compilerList.languages).sort();

  const compilerInfo = useMemo((): CompilerInfo | null => {
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
    <div className="wb-compiler d-flex flex-column gap-16px">
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
          enabledCompilerOptionRaw={
            permlinkData === null
              ? compilerInfo.compilerOptionRaw
              : permlinkData.parameter.compilerInfo.compilerOptionRaw
          }
          compilerOptionRaw={
            permlinkData === null
              ? compilerOptionRaw
              : permlinkData.parameter.compilerOptionRaw
          }
          enabledRuntimeOptionRaw={
            permlinkData === null
              ? compilerInfo.runtimeOptionRaw
              : permlinkData.parameter.compilerInfo.runtimeOptionRaw
          }
          runtimeOptionRaw={
            permlinkData === null
              ? runtimeOptionRaw
              : permlinkData.parameter.runtimeOptionRaw
          }
          readOnly={readOnly}
          onChangeCompilerOptionRaw={onChangeCompilerOptionRaw}
          onChangeRuntimeOptionRaw={onChangeRuntimeOptionRaw}
        />
      )}
    </div>
  );
};

export { Compiler };
