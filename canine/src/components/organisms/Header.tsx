import React from "react";
import makeStyles from "@material-ui/styles/makeStyles";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import AppBar from "@material-ui/core/AppBar";
import Toolbar from "@material-ui/core/Toolbar";
import Typography from "@material-ui/core/Typography";
import deepOrange from "@material-ui/core/colors/deepOrange";

import { Link } from "react-router-dom";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {
    flexGrow: 1
  },
  appBar: {},
  brand: {
    textDecoration: "none",
    flexGrow: 1
  },
  avatar: {
    color: "#fff",
    backgroundColor: deepOrange[500]
  }
}));

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

export const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  const classes = useStyles();

  return (
    <div className={classes.root}>
      <AppBar position="static" className={classes.appBar}>
        <Toolbar>
          <Typography
            className={classes.brand}
            variant="h6"
            color="inherit"
            // component={Link} to="/" だと型が合わなくて怒られる。そのうち直るかもしれない。
            // https://github.com/mui-org/material-ui/issues/8598#issuecomment-399184959
            // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
            component={props => <Link to="/" {...props} />}
          >
            Wandbox
          </Typography>
        </Toolbar>
      </AppBar>
    </div>
  );
};
