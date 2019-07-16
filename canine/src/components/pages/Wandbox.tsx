import React from "react";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { makeStyles } from "@material-ui/styles";

//import { useError } from "~/hooks/error";
import { Header } from "../organisms/Header";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const useStyles = makeStyles((theme: Theme) => ({
  root: theme.mixins.gutters({
    paddingTop: 16,
    paddingBottom: 16,
    marginTop: theme.spacing(3)
  })
}));

export const Wandbox: React.FC<{}> = (): React.ReactElement | null => {
  const classes = useStyles();

  return (
    <React.Fragment>
      <Header />
      <Paper className={classes.root}>
        <Typography variant="h2" gutterBottom>
          Wandbox
        </Typography>
      </Paper>
    </React.Fragment>
  );
};
