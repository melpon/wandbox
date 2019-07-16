import React from "react";
import MuiThemeProvider from "@material-ui/core/styles/MuiThemeProvider";

import { configureTheme } from "~/configureTheme";
import { Error } from "~/hooks/error";
import { AppRouter } from "./AppRouter";
import { AppError } from "./AppError";

const theme = configureTheme(process.env.NODE_ENV || "development");

export const App: React.FC<{}> = (): React.ReactElement => (
  <MuiThemeProvider theme={theme}>
    <Error>
      <AppError>
        <AppRouter />
      </AppError>
    </Error>
  </MuiThemeProvider>
);
