import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import Grid from "@material-ui/core/Grid";
import Button from "@material-ui/core/Button";

import { CodeMirror } from "~/components/organisms/CodeMirror";

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

      <Grid container spacing={2}>
        <Grid item sm={12}>
          <Typography variant="caption">Compiler Option:</Typography>
          {compilerOptionRaw === null ? null : (
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
          )}
        </Grid>
        <Grid item sm={12}>
          <Typography variant="caption">Runtime Option:</Typography>
          {runtimeOptionRaw === null && !expanded ? (
            readOnly ? null : (
              <Button onClick={onExpandRuntimeOptionRaw}>
                Runtime options...
              </Button>
            )
          ) : (
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
              expand={false}
            />
          )}
        </Grid>
      </Grid>
    </Paper>
  );
};

export { RawCompilerOption };
