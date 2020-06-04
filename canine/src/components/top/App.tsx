import React from "react";
import MuiThemeProvider from "@material-ui/core/styles/MuiThemeProvider";
import CssBaseline from "@material-ui/core/CssBaseline";

import { configureTheme } from "~/configureTheme";
import { Error } from "~/hooks/error";
import { AppRouter } from "./AppRouter";
import { AppError } from "./AppError";
import { AppContexts } from "./AppContexts";

const theme = configureTheme(process.env.NODE_ENV || "development");

const App: React.FC = (): React.ReactElement => (
  <AppContexts>
    <CssBaseline />
    <MuiThemeProvider theme={theme}>
      <Error>
        <AppError>
          <AppRouter />
        </AppError>
      </Error>
    </MuiThemeProvider>
  </AppContexts>
);

export { App };
