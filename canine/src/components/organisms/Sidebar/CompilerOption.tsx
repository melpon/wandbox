import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import Divider from "@material-ui/core/Divider";
import Grid from "@material-ui/core/Grid";
import Select from "@material-ui/core/Select";
import MenuItem from "@material-ui/core/MenuItem";
import FormControlLabel from "@material-ui/core/FormControlLabel";
import Checkbox from "@material-ui/core/Checkbox";

import { CompilerInfo, SingleSwitch, SelectSwitch } from "~/hooks/compilerList";

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

interface CompilerOptionProps {
  switches: { [name: string]: string | boolean };
  compilerInfo: CompilerInfo;
  readOnly: boolean;
  onChangeChecked: (switchName: string, checked: boolean) => void;
  onChangeSelected: (switchName: string, selected: string) => void;
}

const CompilerOption: React.FC<CompilerOptionProps> = (
  props
): React.ReactElement => {
  const classes = useStyles();
  const {
    switches,
    compilerInfo,
    readOnly,
    onChangeChecked,
    onChangeSelected
  } = props;

  return (
    <Paper>
      <Typography variant="h6" className={classes.paperTitle}>
        Options
      </Typography>
      <Divider />
      <Grid container className={classes.compilerOptionContainer}>
        {compilerInfo.switches.map(
          (sw): React.ReactElement => {
            if (sw.type === "single") {
              const ssw = sw.switch as SingleSwitch;
              // checkbox
              const checked =
                ssw.name in switches
                  ? (switches[ssw.name] as boolean)
                  : ssw.default;
              return (
                <Grid item sm={12} key={ssw.name}>
                  <FormControlLabel
                    key={ssw.name}
                    control={
                      <Checkbox
                        disabled={readOnly}
                        checked={checked}
                        onChange={(e): void =>
                          onChangeChecked(ssw.name, e.target.checked)
                        }
                        value={ssw.name}
                      />
                    }
                    label={ssw.displayName}
                  />
                </Grid>
              );
            } else if (sw.type === "select") {
              const ssw = sw.switch as SelectSwitch;
              // select
              const value = ((): string => {
                if (!(ssw.name in switches)) {
                  return ssw.default;
                }
                const name = switches[ssw.name];
                if (typeof name !== "string") {
                  return ssw.default;
                }
                if (
                  ssw.options.find((opt): boolean => opt.name === name) ===
                  undefined
                ) {
                  return ssw.default;
                }
                return name;
              })();
              return (
                <Grid item sm={12} key={ssw.name}>
                  <Select
                    disabled={readOnly}
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
                </Grid>
              );
            } else {
              throw "error";
            }
          }
        )}
      </Grid>
    </Paper>
  );
};

export { CompilerOption };
