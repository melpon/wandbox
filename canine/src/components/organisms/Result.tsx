import React from "react";
import Paper from "@material-ui/core/Paper";
import makeStyles from "@material-ui/styles/makeStyles";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { useContainer } from "unstated-next";
import { ResultContext, ResultData } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {
    backgroundColor: "#000000",
  },
  compilerMessageS: {
    color: "#eeeeee",
  },
  compilerMessageE: {
    color: "#e01234",
  },
  stdout: {
    color: "#eeeeee",
  },
  stderr: {
    color: "#e01234",
  },
  control: {
    color: "#12e034",
  },
  signal: {
    color: "#e01234",
  },
  exitcode: {
    color: "#12e034",
  },
}));

interface ResultProps {
  permlinkData: PermlinkData | null;
}

const Result: React.FC<ResultProps> = (props): React.ReactElement => {
  const { permlinkData } = props;
  const classes = useStyles();
  const rs = useContainer(ResultContext);
  const results = permlinkData === null ? rs.results : permlinkData.results;
  const mergedResults: ResultData[] = [];
  let preview: ResultData | null = null;
  for (const r of results) {
    const isMessage =
      r.type === "CompilerMessageS" ||
      r.type === "CompilerMessageE" ||
      r.type === "StdOut" ||
      r.type === "StdErr";

    // 直前と同じメッセージタイプなら結合する
    if (isMessage && preview !== null && preview.type === r.type) {
      mergedResults[mergedResults.length - 1] = {
        type: r.type,
        data: preview.data + r.data,
      };
    } else {
      mergedResults.push(r);
      preview = r;
    }
  }
  const typeClassNames = React.useMemo(
    // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
    () => ({
      CompilerMessageS: classes.compilerMessageS,
      CompilerMessageE: classes.compilerMessageE,
      StdOut: classes.stdout,
      StdErr: classes.stderr,
      Control: classes.control,
      Signal: classes.signal,
      ExitCode: classes.exitcode,
    }),
    []
  );

  return (
    <Paper className={classes.root}>
      <code>
        {mergedResults.map(
          (r, index): React.ReactElement => {
            const className = typeClassNames[r.type];

            return (
              <pre key={index} className={className}>
                {r.data}
              </pre>
            );
          }
        )}
      </code>
    </Paper>
  );
};

export { Result };
