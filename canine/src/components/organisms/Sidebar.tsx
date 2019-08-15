// @flow
import React from "react";
import Select from "@material-ui/core/Select";
import MenuItem from "@material-ui/core/MenuItem";
import FormControlLabel from "@material-ui/core/FormControlLabel";
import Checkbox from "@material-ui/core/Checkbox";
import Button from "@material-ui/core/Button";

import { CompilerList, SelectSwitchOption } from "~/hooks/compilerList";
import { CompilerContext } from "~/contexts/CompilerContext";
import { SingleSwitch, SelectSwitch } from "~/hooks/compilerList";
import { CodeMirror } from "./CodeMirror";
import { PermlinkData } from "~/hooks/permlink";

interface SidebarProps {
  //editor: EditorState,
  //compiler: CompilerState,
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

export const Sidebar: React.FC<SidebarProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const compilerContext = CompilerContext.useContainer();
  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    runtimeOptionRawExpanded
  } = compilerContext;
  const onChangeLanguage = React.useCallback((e): void => {
    const language = e.target.value;
    compilerContext.setCurrentLanguage(language);
  }, []);
  const onChangeCompiler = React.useCallback((e): void => {
    const compiler = e.target.value;
    compilerContext.setCurrentCompilerName(compiler);
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
  const onExpandRuntimeOptionRaw = React.useCallback((): void => {
    compilerContext.setRuntimeOptionRawExpanded(true);
  }, []);
  const onCtrlEnter = React.useCallback((): void => {}, []);

  const languages = Object.keys(compilerList.languages).sort();

  if (permlinkData === null) {
    return (
      <React.Fragment>
        {/* choose language */}
        <Select value={currentLanguage} onChange={onChangeLanguage}>
          {languages.map(
            (lang): React.ReactElement => {
              return (
                <MenuItem key={lang} value={lang}>
                  {lang}
                </MenuItem>
              );
            }
          )}
        </Select>

        {/* choose compiler */}
        {((): React.ReactElement | null => {
          if (currentLanguage === "") {
            return null;
          }

          const infos = compilerList.languages[currentLanguage];
          if (infos === undefined) {
            return null;
          }

          return (
            <Select value={currentCompilerName} onChange={onChangeCompiler}>
              {infos.map(
                (info): React.ReactElement => {
                  return (
                    <MenuItem key={info.name} value={info.name}>
                      {`${info.displayName} ${info.version}`}
                    </MenuItem>
                  );
                }
              )}
            </Select>
          );
        })()}

        {/* compiler options */}
        {((): React.ReactElement | null => {
          if (currentCompilerName === "") {
            return null;
          }

          const info = compilerList.compilers.find(
            (compiler): boolean => compiler.name === currentCompilerName
          );
          if (info === undefined) {
            return null;
          }

          return (
            <React.Fragment>
              {info.switches.map(
                (sw): React.ReactElement => {
                  if (sw.type === "single") {
                    const ssw = sw.switch as SingleSwitch;
                    // checkbox
                    const checked =
                      ssw.name in currentSwitches
                        ? (currentSwitches[ssw.name] as boolean)
                        : ssw.default;
                    return (
                      <FormControlLabel
                        key={ssw.name}
                        control={
                          <Checkbox
                            checked={checked}
                            onChange={(e): void =>
                              onChangeChecked(ssw.name, e.target.checked)
                            }
                            value={ssw.name}
                          />
                        }
                        label={ssw.displayName}
                      />
                    );
                  } else if (sw.type === "select") {
                    const ssw = sw.switch as SelectSwitch;
                    // select
                    const value = ((): string => {
                      if (!(ssw.name in currentSwitches)) {
                        return ssw.default;
                      }
                      const name = currentSwitches[ssw.name];
                      if (typeof name !== "string") {
                        return ssw.default;
                      }
                      if (
                        ssw.options.find(
                          (opt): boolean => opt.name === name
                        ) === undefined
                      ) {
                        return ssw.default;
                      }
                      return name;
                    })();
                    return (
                      <Select
                        key={ssw.name}
                        value={value}
                        onChange={(
                          e: React.ChangeEvent<{
                            name?: string;
                            value: unknown;
                          }>
                        ): void =>
                          onChangeSelected(ssw.name, e.target.value as string)
                        }
                      >
                        {ssw.options.map(
                          (opt): React.ReactElement => {
                            return (
                              <MenuItem key={opt.name} value={opt.name}>
                                {opt.displayName}
                              </MenuItem>
                            );
                          }
                        )}
                      </Select>
                    );
                  } else {
                    throw "error";
                  }
                }
              )}
            </React.Fragment>
          );
        })()}

        {/* compiler/runtime options raw */}
        {((): React.ReactElement | null => {
          if (currentCompilerName === "") {
            return null;
          }

          const info = compilerList.compilers.find(
            (compiler): boolean => compiler.name === currentCompilerName
          );
          if (info === undefined) {
            return null;
          }

          let compilerComponent = null;
          if (info.compilerOptionRaw) {
            compilerComponent = (
              <CodeMirror
                value={compilerOptionRaw}
                options={{
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeCompilerOptionRaw}
                expand={false}
              />
            );
          }

          let runtimeComponent = null;
          if (info.runtimeOptionRaw || runtimeOptionRawExpanded) {
            runtimeComponent = (
              <CodeMirror
                value={runtimeOptionRaw}
                options={{
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeRuntimeOptionRaw}
                expand={false}
              />
            );
          } else {
            runtimeComponent = (
              <Button onClick={onExpandRuntimeOptionRaw}>
                Runtime options...
              </Button>
            );
          }
          return (
            <div>
              {compilerComponent}
              {runtimeComponent}
            </div>
          );
        })()}
      </React.Fragment>
    );
  } else {
    // パーマリングが有効な場合、変更不可にして permlinkData から構築する
    const { compiler, compilerInfo } = permlinkData.parameter;

    return (
      <React.Fragment>
        {/* choose language */}
        <Select disabled value={compilerInfo.language}>
          <MenuItem value={compilerInfo.language}>
            {compilerInfo.language}
          </MenuItem>
        </Select>

        {/* choose compiler */}
        {((): React.ReactElement | null => {
          return (
            <Select disabled value={compiler}>
              <MenuItem value={compilerInfo.name}>
                {`${compilerInfo.displayName} ${compilerInfo.version}`}
              </MenuItem>
            </Select>
          );
        })()}

        {/* compiler options */}
        {((): React.ReactElement | null => {
          const options = permlinkData.parameter.options.split(",");
          return (
            <React.Fragment>
              {compilerInfo.switches.map(
                (sw): React.ReactElement => {
                  if (sw.type === "single") {
                    const ssw = sw.switch as SingleSwitch;
                    // checkbox
                    const checked =
                      options.findIndex((x): boolean => x === ssw.name) !== -1;
                    return (
                      <FormControlLabel
                        key={ssw.name}
                        control={
                          <Checkbox
                            disabled
                            checked={checked}
                            value={ssw.name}
                          />
                        }
                        label={ssw.displayName}
                      />
                    );
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
                    return (
                      <Select disabled key={ssw.name} value={value.name}>
                        <MenuItem value={value.name}>
                          {value.displayName}
                        </MenuItem>
                      </Select>
                    );
                  } else {
                    throw "error";
                  }
                }
              )}
            </React.Fragment>
          );
        })()}

        {/* compiler/runtime options raw */}
        {((): React.ReactElement | null => {
          const {
            compilerOptionRaw,
            runtimeOptionRaw
          } = permlinkData.parameter;

          let compilerComponent = null;
          if (compilerInfo.compilerOptionRaw) {
            compilerComponent = (
              <CodeMirror
                value={compilerOptionRaw}
                options={{
                  readOnly: true,
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeCompilerOptionRaw}
                expand={false}
              />
            );
          }

          let runtimeComponent = null;
          if (compilerInfo.runtimeOptionRaw || runtimeOptionRaw.length !== 0) {
            runtimeComponent = (
              <CodeMirror
                value={runtimeOptionRaw}
                options={{
                  readOnly: true,
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeRuntimeOptionRaw}
                expand={false}
              />
            );
          } else {
            runtimeComponent = null;
          }
          return (
            <div>
              {compilerComponent}
              {runtimeComponent}
            </div>
          );
        })()}
      </React.Fragment>
    );
  }
};
