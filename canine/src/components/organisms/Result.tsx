import React from "react";
import Paper from "@material-ui/core/Paper";
import makeStyles from "@material-ui/styles/makeStyles";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { useContainer } from "unstated-next";
import { ResultContext } from "~/contexts/ResultContext";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {
    flexGrow: 1
  }
}));

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface ResultProps {}

export const Result: React.FC<ResultProps> = (): React.ReactElement => {
  const classes = useStyles();
  const rs = useContainer(ResultContext);

  return (
    <Paper className={classes.root}>
      <code>
        {rs.results.map(
          (r, index): React.ReactElement => {
            return <pre key={index}>{JSON.stringify(r)}</pre>;
          }
        )}
      </code>
    </Paper>
  );
};
