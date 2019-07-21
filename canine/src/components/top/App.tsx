import React from "react";
import MuiThemeProvider from "@material-ui/core/styles/MuiThemeProvider";

import { configureTheme } from "~/configureTheme";
import { CompilerContext } from "~/contexts/CompilerContext";
import { Error } from "~/hooks/error";
import { AppRouter } from "./AppRouter";
import { AppError } from "./AppError";

const theme = configureTheme(process.env.NODE_ENV || "development");

export const App: React.FC<{}> = (): React.ReactElement => (
  <CompilerContext.Provider>
    <MuiThemeProvider theme={theme}>
      <Error>
        <AppError>
          <AppRouter />
        </AppError>
      </Error>
    </MuiThemeProvider>
  </CompilerContext.Provider>
);
