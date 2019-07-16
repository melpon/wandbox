import createMuiTheme, { Theme } from "@material-ui/core/styles/createMuiTheme";
import indigo from "@material-ui/core/colors/indigo";
import pink from "@material-ui/core/colors/pink";
import red from "@material-ui/core/colors/red";
import lime from "@material-ui/core/colors/lime";

export const configureTheme = (env: string): Theme => {
  const t = {
    production: {
      palette: {
        primary: red,
        secondary: lime
      }
    },
    development: {
      palette: {
        primary: indigo,
        secondary: pink
      }
    }
  };
  if (!(env in t)) {
    return createMuiTheme(t["development"]);
  }

  return createMuiTheme(t[env as keyof typeof t]);
};
