import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import makeStyles from "@material-ui/styles/makeStyles";
import List from "@material-ui/core/List";
import ListItem from "@material-ui/core/ListItem";
import ListItemText from "@material-ui/core/ListItemText";
import Typography from "@material-ui/core/Typography";
import Paper from "@material-ui/core/Paper";
import Divider from "@material-ui/core/Divider";
import ListItemSecondaryAction from "@material-ui/core/ListItemSecondaryAction";
import IconButton from "@material-ui/core/IconButton";
import ClearIcon from "@material-ui/icons/Clear";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type, @typescript-eslint/no-unused-vars
const useStyles = makeStyles((theme: Theme) => ({
  root: {},
  paperTitle: {
    paddingLeft: "8px",
  },
  languageList: {
    paddingTop: "0px",
    paddingBottom: "0px",
  },
  languageListItem: {
    paddingLeft: "16px",
  },
  compilerList: {
    paddingTop: "0px",
    paddingBottom: "0px",
  },
  compilerListItem: {
    paddingLeft: "16px",
  },
  compilerListItemText: {
    whiteSpace: "nowrap",
    textOverflow: "ellipsis",
    overflowX: "hidden",
  },
  compilerOptionContainer: {
    paddingLeft: "16px",
    paddingRight: "16px",
    paddingBottom: "16px",
  },
}));

interface ChooseLanguageProps {
  language: string | null;
  languages: string[];
  readOnly: boolean;
  onSelectLanguage: (language: string) => void;
  onDeselectLanguage: () => void;
}

const ChooseLanguage: React.FC<ChooseLanguageProps> = (
  props
): React.ReactElement => {
  const classes = useStyles();
  const {
    language,
    languages,
    readOnly,
    onSelectLanguage,
    onDeselectLanguage,
  } = props;

  return (
    <Paper>
      <Typography variant="h6" className={classes.paperTitle}>
        Language
      </Typography>
      {language === null ? (
        <List dense className={classes.languageList}>
          {languages.map(
            (lang): React.ReactElement => {
              return (
                <React.Fragment key={lang}>
                  <Divider />
                  <ListItem
                    className={classes.languageListItem}
                    button
                    onClick={(): void => onSelectLanguage(lang)}
                  >
                    <ListItemText primary={lang} />
                  </ListItem>
                </React.Fragment>
              );
            }
          )}
        </List>
      ) : (
        <List dense className={classes.languageList}>
          <Divider />
          <ListItem className={classes.languageListItem}>
            <ListItemText primary={language} />
            {readOnly ? null : (
              <ListItemSecondaryAction>
                <IconButton onClick={onDeselectLanguage}>
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

export { ChooseLanguage };
