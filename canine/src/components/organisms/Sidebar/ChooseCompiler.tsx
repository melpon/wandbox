import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import List from "@material-ui/core/List";
import ListItem from "@material-ui/core/ListItem";
import ListItemText from "@material-ui/core/ListItemText";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import Divider from "@material-ui/core/Divider";
import ClearIcon from "@material-ui/icons/Clear";
import ListItemSecondaryAction from "@material-ui/core/ListItemSecondaryAction";
import IconButton from "@material-ui/core/IconButton";

import { CompilerInfo } from "~/hooks/compilerList";

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

interface ChooseCompilerProps {
  compilerInfo: CompilerInfo | null;
  compilerInfos: CompilerInfo[];
  readOnly: boolean;
  onSelectCompiler: (compiler: CompilerInfo) => void;
  onDeselectCompiler: () => void;
}

const ChooseCompiler: React.FC<ChooseCompilerProps> = (
  props
): React.ReactElement => {
  const classes = useStyles();
  const {
    compilerInfo,
    compilerInfos,
    readOnly,
    onSelectCompiler,
    onDeselectCompiler
  } = props;

  return (
    <Paper>
      <Typography variant="h6" className={classes.paperTitle}>
        Compiler
      </Typography>
      {compilerInfo === null ? (
        <List dense className={classes.compilerList}>
          {compilerInfos.map(
            (info): React.ReactElement => {
              return (
                <React.Fragment key={info.name}>
                  <Divider />
                  <ListItem
                    className={classes.compilerListItem}
                    button
                    onClick={(): void => onSelectCompiler(info)}
                  >
                    <ListItemText
                      className={classes.compilerListItemText}
                      primary={`${info.displayName} ${info.version}`}
                    />
                  </ListItem>
                </React.Fragment>
              );
            }
          )}
        </List>
      ) : (
        <List dense className={classes.languageList}>
          <Divider />
          <ListItem className={classes.compilerListItem}>
            <ListItemText
              className={classes.compilerListItemText}
              primary={`${compilerInfo.displayName} ${compilerInfo.version}`}
            />
            {readOnly ? null : (
              <ListItemSecondaryAction>
                <IconButton onClick={onDeselectCompiler}>
                  <ClearIcon />
                </IconButton>
              </ListItemSecondaryAction>
            )}
          </ListItem>
        </List>
      )}
    </Paper>
  );
};

export { ChooseCompiler };
