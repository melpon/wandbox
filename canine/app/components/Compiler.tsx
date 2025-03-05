import React, { useCallback, useEffect, useMemo } from "react";
import { useSelector } from "react-redux";

import type { CompilerList, CompilerInfo } from "~/hooks/compilerList";
import type { PermlinkData } from "~/hooks/permlink";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { MergedHppInfo, wandboxSlice } from "~/features/slice";
import { useGetTemplate } from "~/hooks/template";
import { useError } from "~/hooks/error";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { ChooseLanguage } from "./Compiler/ChooseLanguage";
import { ChooseCompiler } from "./Compiler/ChooseCompiler";
import { CompilerOption } from "./Compiler/CompilerOption";
import { RawCompilerOption } from "./Compiler/RawCompilerOption";
import { optionsToSwitch } from "~/utils/optionsToSwitch";

interface CompilerProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
  hpplib: MergedHppInfo[];
}

const Compiler: React.FC<CompilerProps> = (props) => {
  const { compilerList, permlinkData, hpplib } = props;
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

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const [_, setError] = useError();
  const [loadedTemplate, templateFetchId, doLoadTemplate] = useGetTemplate(
    "",
    setError
  );

  const onSelectLanguage = useCallback((language: string): void => {
    dispatch(actions.setCurrentLanguage(language));
    // 言語を選択するとコンパイラ名を自動で設定済みにする
    // head 以外のコンパイラを設定する
    let name: string | null = null;
    for (const ci of compilerList.languages[language]) {
      if (ci.name.indexOf("head") === -1) {
        name = ci.name;
        break;
      }
    }
    // ただし全部 head だったら諦めて head を使う
    if (name === null) {
      name = compilerList.languages[language][0].name;
    }
    dispatch(actions.setCurrentCompilerName(name));
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
  const onLoadTemplate = useCallback((template: string): void => {
    doLoadTemplate(`/api/template/${template}`, {});
  }, []);

  useEffect(() => {
    if (loadedTemplate === null) {
      return;
    }

    dispatch(
      actions.initSources(
        createEditorSourceData(loadedTemplate.code, loadedTemplate.codes)
      )
    );
  }, [templateFetchId]);

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
          templates={compilerInfo === null ? [] : compilerInfo.templates}
          hpplib={hpplib}
          onSelectCompiler={onSelectCompiler}
          onDeselectCompiler={onDeselectCompiler}
          onLoadTemplate={onLoadTemplate}
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
