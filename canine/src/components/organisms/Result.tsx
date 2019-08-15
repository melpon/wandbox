import React from "react";
import Paper from "@material-ui/core/Paper";
import makeStyles from "@material-ui/styles/makeStyles";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { useContainer } from "unstated-next";
import { ResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {
    flexGrow: 1
  }
}));

interface ResultProps {
  permlinkData: PermlinkData | null;
}

export const Result: React.FC<ResultProps> = (props): React.ReactElement => {
  const { permlinkData } = props;
  const classes = useStyles();
  const rs = useContainer(ResultContext);
  const results = permlinkData === null ? rs.results : permlinkData.results;

  return (
    <Paper className={classes.root}>
      <code>
        {results.map(
          (r, index): React.ReactElement => {
            return <pre key={index}>{JSON.stringify(r)}</pre>;
          }
        )}
      </code>
    </Paper>
  );
};
