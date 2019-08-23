import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import Grid from "@material-ui/core/Grid";
import Button from "@material-ui/core/Button";
import Divider from "@material-ui/core/Divider";

import { CodeMirror } from "~/components/organisms/CodeMirror";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  paperTitle: {
    paddingLeft: "8px"
  },
  optionTitle: {
    paddingLeft: "10px"
  }
}));

interface RawCompilerOptionProps {
  compilerOptionRaw: string | null;
  runtimeOptionRaw: string | null;
  readOnly: boolean;
  onChangeCompilerOptionRaw: (
    cm: unknown,
    data: unknown,
    value: string
  ) => void;
  onChangeRuntimeOptionRaw: (cm: unknown, data: unknown, value: string) => void;
  onCtrlEnter: () => void;
}

const RawCompilerOption: React.FC<RawCompilerOptionProps> = (
  props
): React.ReactElement => {
  const classes = useStyles();
  const {
    compilerOptionRaw,
    runtimeOptionRaw,
    readOnly,
    onChangeCompilerOptionRaw,
    onChangeRuntimeOptionRaw,
    onCtrlEnter
  } = props;
  const [expanded, setExpanded] = React.useState<boolean>(false);
  const onExpandRuntimeOptionRaw = React.useCallback((): void => {
    setExpanded(true);
  }, []);

  return (
    <Paper>
      <Typography variant="h6" className={classes.paperTitle}>
        Raw Options
      </Typography>
      <Divider />

      <Grid container spacing={2}>
        <Grid item sm={12}>
          {compilerOptionRaw === null ? null : (
            <React.Fragment>
              <Typography variant="caption" className={classes.optionTitle}>
                Compiler Option:
              </Typography>
              <CodeMirror
                value={compilerOptionRaw}
                options={{
                  readOnly: readOnly,
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeCompilerOptionRaw}
                style="input"
              />
            </React.Fragment>
          )}
        </Grid>
        <Grid item sm={12}>
          {runtimeOptionRaw === null && !expanded ? (
            readOnly ? null : (
              <Button onClick={onExpandRuntimeOptionRaw}>
                Runtime options...
              </Button>
            )
          ) : (
            <React.Fragment>
              <Typography variant="caption" className={classes.optionTitle}>
                Runtime Option:
              </Typography>
              <CodeMirror
                value={runtimeOptionRaw === null ? "" : runtimeOptionRaw}
                options={{
                  readOnly: readOnly,
                  viewportMargin: Infinity,
                  smartIndent: false,
                  extraKeys: {
                    "Ctrl-Enter": (): void => {
                      onCtrlEnter();
                    }
                  }
                }}
                onBeforeChange={onChangeRuntimeOptionRaw}
                style="input"
              />
            </React.Fragment>
          )}
        </Grid>
      </Grid>
    </Paper>
  );
};

export { RawCompilerOption };
