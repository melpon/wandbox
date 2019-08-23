import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";

import {
  CompilerList,
  SelectSwitchOption,
  CompilerInfo
} from "~/hooks/compilerList";
import { CompilerContext } from "~/contexts/CompilerContext";
import { SingleSwitch, SelectSwitch } from "~/hooks/compilerList";
import { PermlinkData } from "~/hooks/permlink";
import Grid from "@material-ui/core/Grid";
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
  const compilerContext = CompilerContext.useContainer();
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
  const onCtrlEnter = React.useCallback((): void => {}, []);

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

  const compilerInfos: CompilerInfo[] | undefined =
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
        {currentLanguage === "" || compilerInfos === undefined ? null : (
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
                ? compilerInfo.runtimeOptionRaw
                  ? runtimeOptionRaw
                  : null
                : permlinkData.parameter.compilerInfo.runtimeOptionRaw
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

  //if (permlinkData === null) {
  //  return (
  //    <Grid container className={classes.root} spacing={2}>
  //      {/* choose language */}
  //      <Grid item sm={12}>
  //        <Paper>
  //          <Typography variant="h6" className={classes.paperTitle}>
  //            Language
  //          </Typography>
  //          {currentLanguage === "" ? (
  //            <List dense className={classes.languageList}>
  //              {languages.map(
  //                (lang): React.ReactElement => {
  //                  return (
  //                    <React.Fragment key={lang}>
  //                      <Divider />
  //                      <ListItem
  //                        className={classes.languageListItem}
  //                        button
  //                        onClick={(): void => onSelectLanguage(lang)}
  //                      >
  //                        <ListItemText primary={lang} />
  //                      </ListItem>
  //                    </React.Fragment>
  //                  );
  //                }
  //              )}
  //            </List>
  //          ) : (
  //            <List dense className={classes.languageList}>
  //              <Divider />
  //              <ListItem
  //                className={classes.languageListItem}
  //                button
  //                onClick={onDeselectLanguage}
  //              >
  //                <ListItemText primary={currentLanguage} />
  //              </ListItem>
  //            </List>
  //          )}
  //        </Paper>
  //      </Grid>
  //      {/* choose compiler */}
  //      <Grid item sm={12}>
  //        {((): React.ReactElement | null => {
  //          if (currentLanguage === "") {
  //            return null;
  //          }

  //          const infos = compilerList.languages[currentLanguage];
  //          if (infos === undefined) {
  //            return null;
  //          }

  //          return (
  //            <Paper>
  //              <Typography variant="h6" className={classes.paperTitle}>
  //                Compiler
  //              </Typography>
  //              {currentCompilerInfo === null ? (
  //                <List dense className={classes.compilerList}>
  //                  {infos.map(
  //                    (info): React.ReactElement => {
  //                      return (
  //                        <React.Fragment key={info.name}>
  //                          <Divider />
  //                          <ListItem
  //                            className={classes.compilerListItem}
  //                            button
  //                            onClick={(): void => onSelectCompiler(info.name)}
  //                          >
  //                            <ListItemText
  //                              className={classes.compilerListItemText}
  //                              primary={`${info.displayName} ${info.version}`}
  //                            />
  //                          </ListItem>
  //                        </React.Fragment>
  //                      );
  //                    }
  //                  )}
  //                </List>
  //              ) : (
  //                <List dense className={classes.languageList}>
  //                  <Divider />
  //                  <ListItem
  //                    className={classes.compilerListItem}
  //                    button
  //                    onClick={onDeselectCompiler}
  //                  >
  //                    <ListItemText
  //                      className={classes.compilerListItemText}
  //                      primary={`${currentCompilerInfo.displayName} ${currentCompilerInfo.version}`}
  //                    />
  //                  </ListItem>
  //                </List>
  //              )}
  //            </Paper>
  //          );
  //        })()}
  //      </Grid>

  //      {/* compiler options */}
  //      <Grid item sm={12}>
  //        {((): React.ReactElement | null => {
  //          if (currentCompilerName === "") {
  //            return null;
  //          }

  //          const info = currentCompilerInfo;
  //          if (info === null) {
  //            return null;
  //          }

  //          return (
  //            <Paper>
  //              <Typography variant="h6" className={classes.paperTitle}>
  //                Options
  //              </Typography>
  //              <Divider />
  //              <Grid container className={classes.compilerOptionContainer}>
  //                {info.switches.map(
  //                  (sw): React.ReactElement => {
  //                    if (sw.type === "single") {
  //                      const ssw = sw.switch as SingleSwitch;
  //                      // checkbox
  //                      const checked =
  //                        ssw.name in currentSwitches
  //                          ? (currentSwitches[ssw.name] as boolean)
  //                          : ssw.default;
  //                      return (
  //                        <Grid item sm={12}>
  //                          <FormControlLabel
  //                            key={ssw.name}
  //                            control={
  //                              <Checkbox
  //                                checked={checked}
  //                                onChange={(e): void =>
  //                                  onChangeChecked(ssw.name, e.target.checked)
  //                                }
  //                                value={ssw.name}
  //                              />
  //                            }
  //                            label={ssw.displayName}
  //                          />
  //                        </Grid>
  //                      );
  //                    } else if (sw.type === "select") {
  //                      const ssw = sw.switch as SelectSwitch;
  //                      // select
  //                      const value = ((): string => {
  //                        if (!(ssw.name in currentSwitches)) {
  //                          return ssw.default;
  //                        }
  //                        const name = currentSwitches[ssw.name];
  //                        if (typeof name !== "string") {
  //                          return ssw.default;
  //                        }
  //                        if (
  //                          ssw.options.find(
  //                            (opt): boolean => opt.name === name
  //                          ) === undefined
  //                        ) {
  //                          return ssw.default;
  //                        }
  //                        return name;
  //                      })();
  //                      return (
  //                        <Grid item sm={12}>
  //                          <Select
  //                            key={ssw.name}
  //                            value={value}
  //                            onChange={(
  //                              e: React.ChangeEvent<{
  //                                name?: string;
  //                                value: unknown;
  //                              }>
  //                            ): void =>
  //                              onChangeSelected(ssw.name, e.target
  //                                .value as string)
  //                            }
  //                          >
  //                            {ssw.options.map(
  //                              (opt): React.ReactElement => {
  //                                return (
  //                                  <MenuItem key={opt.name} value={opt.name}>
  //                                    {opt.displayName}
  //                                  </MenuItem>
  //                                );
  //                              }
  //                            )}
  //                          </Select>
  //                        </Grid>
  //                      );
  //                    } else {
  //                      throw "error";
  //                    }
  //                  }
  //                )}
  //              </Grid>
  //            </Paper>
  //          );
  //        })()}
  //      </Grid>

  //      {/* compiler/runtime options raw */}
  //      {((): React.ReactElement | null => {
  //        if (currentCompilerName === "") {
  //          return null;
  //        }

  //        const info = compilerList.compilers.find(
  //          (compiler): boolean => compiler.name === currentCompilerName
  //        );
  //        if (info === undefined) {
  //          return null;
  //        }

  //        let compilerComponent = null;
  //        if (info.compilerOptionRaw) {
  //          compilerComponent = (
  //            <CodeMirror
  //              value={compilerOptionRaw}
  //              options={{
  //                viewportMargin: Infinity,
  //                smartIndent: false,
  //                extraKeys: {
  //                  "Ctrl-Enter": (): void => {
  //                    onCtrlEnter();
  //                  }
  //                }
  //              }}
  //              onBeforeChange={onChangeCompilerOptionRaw}
  //              expand={false}
  //            />
  //          );
  //        }

  //        let runtimeComponent = null;
  //        if (info.runtimeOptionRaw || runtimeOptionRawExpanded) {
  //          runtimeComponent = (
  //            <CodeMirror
  //              value={runtimeOptionRaw}
  //              options={{
  //                viewportMargin: Infinity,
  //                smartIndent: false,
  //                extraKeys: {
  //                  "Ctrl-Enter": (): void => {
  //                    onCtrlEnter();
  //                  }
  //                }
  //              }}
  //              onBeforeChange={onChangeRuntimeOptionRaw}
  //              expand={false}
  //            />
  //          );
  //        } else {
  //          runtimeComponent = (
  //            <Button onClick={onExpandRuntimeOptionRaw}>
  //              Runtime options...
  //            </Button>
  //          );
  //        }
  //        return (
  //          <Grid item sm={12}>
  //            <Paper>
  //              <Typography variant="h6" className={classes.paperTitle}>
  //                Raw Options
  //              </Typography>

  //              <Grid container spacing={2}>
  //                <Grid item sm={12}>
  //                  {compilerComponent}
  //                </Grid>
  //                <Grid item sm={12}>
  //                  {runtimeComponent}
  //                </Grid>
  //              </Grid>
  //            </Paper>
  //          </Grid>
  //        );
  //      })()}
  //    </Grid>
  //  );
  //} else {
  //  // パーマリングが有効な場合、変更不可にして permlinkData から構築する
  //  const { compiler, compilerInfo } = permlinkData.parameter;

  //  return (
  //    <Grid container>
  //      {/* choose language */}
  //      <Grid item sm={12}>
  //        <Select disabled value={compilerInfo.language}>
  //          <MenuItem value={compilerInfo.language}>
  //            {compilerInfo.language}
  //          </MenuItem>
  //        </Select>
  //      </Grid>

  //      {/* choose compiler */}
  //      <Grid item sm={12}>
  //        {((): React.ReactElement | null => {
  //          return (
  //            <Select disabled value={compiler}>
  //              <MenuItem value={compilerInfo.name}>
  //                {`${compilerInfo.displayName} ${compilerInfo.version}`}
  //              </MenuItem>
  //            </Select>
  //          );
  //        })()}
  //      </Grid>

  //      {/* compiler options */}
  //      <Grid item sm={12}>
  //        {((): React.ReactElement | null => {
  //          const options = permlinkData.parameter.options.split(",");
  //          return (
  //            <Grid container>
  //              {compilerInfo.switches.map(
  //                (sw): React.ReactElement => {
  //                  if (sw.type === "single") {
  //                    const ssw = sw.switch as SingleSwitch;
  //                    // checkbox
  //                    const checked =
  //                      options.findIndex((x): boolean => x === ssw.name) !==
  //                      -1;
  //                    return (
  //                      <Grid item sm={12}>
  //                        <FormControlLabel
  //                          key={ssw.name}
  //                          control={
  //                            <Checkbox
  //                              disabled
  //                              checked={checked}
  //                              value={ssw.name}
  //                            />
  //                          }
  //                          label={ssw.displayName}
  //                        />
  //                      </Grid>
  //                    );
  //                  } else if (sw.type === "select") {
  //                    const ssw = sw.switch as SelectSwitch;
  //                    // select
  //                    const value = ((): SelectSwitchOption => {
  //                      // ssw.options の中から options に含まれるオプションを探す。
  //                      // 多分複数一致することは無いはずだし、複数あってもどうしようも無いので
  //                      // 最初に一致したものを返す。
  //                      for (const opt of ssw.options) {
  //                        for (const target of options) {
  //                          if (opt.name === target) {
  //                            return opt;
  //                          }
  //                        }
  //                      }

  //                      // ここに来ることは無いはず
  //                      throw "おかしい";
  //                    })();
  //                    return (
  //                      <Grid item sm={12}>
  //                        <Select disabled key={ssw.name} value={value.name}>
  //                          <MenuItem value={value.name}>
  //                            {value.displayName}
  //                          </MenuItem>
  //                        </Select>
  //                      </Grid>
  //                    );
  //                  } else {
  //                    throw "error";
  //                  }
  //                }
  //              )}
  //            </Grid>
  //          );
  //        })()}
  //      </Grid>

  //      {/* compiler/runtime options raw */}
  //      <Grid item sm={12}>
  //        {((): React.ReactElement | null => {
  //          const {
  //            compilerOptionRaw,
  //            runtimeOptionRaw
  //          } = permlinkData.parameter;

  //          let compilerComponent = null;
  //          if (compilerInfo.compilerOptionRaw) {
  //            compilerComponent = (
  //              <CodeMirror
  //                value={compilerOptionRaw}
  //                options={{
  //                  readOnly: true,
  //                  viewportMargin: Infinity,
  //                  smartIndent: false,
  //                  extraKeys: {
  //                    "Ctrl-Enter": (): void => {
  //                      onCtrlEnter();
  //                    }
  //                  }
  //                }}
  //                onBeforeChange={onChangeCompilerOptionRaw}
  //                expand={false}
  //              />
  //            );
  //          }

  //          let runtimeComponent = null;
  //          if (
  //            compilerInfo.runtimeOptionRaw ||
  //            runtimeOptionRaw.length !== 0
  //          ) {
  //            runtimeComponent = (
  //              <CodeMirror
  //                value={runtimeOptionRaw}
  //                options={{
  //                  readOnly: true,
  //                  viewportMargin: Infinity,
  //                  smartIndent: false,
  //                  extraKeys: {
  //                    "Ctrl-Enter": (): void => {
  //                      onCtrlEnter();
  //                    }
  //                  }
  //                }}
  //                onBeforeChange={onChangeRuntimeOptionRaw}
  //                expand={false}
  //              />
  //            );
  //          } else {
  //            runtimeComponent = null;
  //          }
  //          return (
  //            <Grid container>
  //              <Grid item sm={12}>
  //                {compilerComponent}
  //              </Grid>
  //              <Grid item sm={12}>
  //                {runtimeComponent}
  //              </Grid>
  //            </Grid>
  //          );
  //        })()}
  //      </Grid>
  //    </Grid>
  //  );
  //}
};

export { Sidebar };
