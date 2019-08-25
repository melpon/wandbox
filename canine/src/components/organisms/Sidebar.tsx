import React from "react";
import { useContainer } from "unstated-next";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import Grid from "@material-ui/core/Grid";

import {
  CompilerList,
  SelectSwitchOption,
  CompilerInfo
} from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { CompilerContext } from "~/contexts/CompilerContext";
import { EditorContext } from "~/contexts/EditorContext";
import { ResultContext } from "~/contexts/ResultContext";
import { SingleSwitch, SelectSwitch } from "~/hooks/compilerList";
import { ChooseLanguage } from "./Sidebar/ChooseLanguage";
import { ChooseCompiler } from "./Sidebar/ChooseCompiler";
import { CompilerOption } from "./Sidebar/CompilerOption";
import { RawCompilerOption } from "./Sidebar/RawCompilerOption";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {},
  paperTitle: {
    paddingLeft: "8px"
  },
  languageList: {
    paddingTop: "0px",
    paddingBottom: "0px"
  },
  languageListItem: {
    paddingLeft: "16px"
  },
  compilerList: {
    paddingTop: "0px",
    paddingBottom: "0px"
  },
  compilerListItem: {
    paddingLeft: "16px"
  },
  compilerListItemText: {
    whiteSpace: "nowrap",
    textOverflow: "ellipsis",
    overflowX: "hidden"
  },
  compilerOptionContainer: {
    paddingLeft: "16px",
    paddingRight: "16px",
    paddingBottom: "16px"
  }
}));

interface SidebarProps {
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
  let switches: { [name: string]: string | boolean } = {};
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

const Sidebar: React.FC<SidebarProps> = (props): React.ReactElement => {
  const classes = useStyles();
  const { compilerList, permlinkData } = props;
  const editorContext = useContainer(EditorContext);
  const compilerContext = useContainer(CompilerContext);
  const resultContext = useContainer(ResultContext);
  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw
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
      compilerContext.setCurrentSwitches(opts => ({
        ...opts,
        [switchName]: checked
      }));
    },
    []
  );
  const onChangeSelected = React.useCallback(
    (switchName: string, selected: string): void => {
      // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
      compilerContext.setCurrentSwitches(opts => ({
        ...opts,
        [switchName]: selected
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
    <Grid container className={classes.root} spacing={2}>
      {/* choose language */}
      <Grid item sm={12}>
        <ChooseLanguage
          language={language}
          languages={languages}
          readOnly={readOnly}
          onSelectLanguage={onSelectLanguage}
          onDeselectLanguage={onDeselectLanguage}
        />
      </Grid>
      {/* choose compiler */}
      <Grid item sm={12}>
        {permlinkData === null && currentLanguage === "" ? null : (
          <ChooseCompiler
            compilerInfo={compilerInfo}
            compilerInfos={compilerInfos}
            readOnly={readOnly}
            onSelectCompiler={onSelectCompiler}
            onDeselectCompiler={onDeselectCompiler}
          />
        )}
      </Grid>
      {/* compiler options */}
      <Grid item sm={12}>
        {compilerInfo === null ? null : (
          <CompilerOption
            switches={switches}
            compilerInfo={compilerInfo}
            readOnly={readOnly}
            onChangeChecked={onChangeChecked}
            onChangeSelected={onChangeSelected}
          />
        )}
      </Grid>
      {/* raw compiler options */}
      <Grid item sm={12}>
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
      </Grid>
    </Grid>
  );
};

export { Sidebar };
