import React from "react";

import { Error } from "~/hooks/error";
import { AppRouter } from "./AppRouter";
import { AppError } from "./AppError";
import { AppContexts } from "./AppContexts";

const App: React.FC = (): React.ReactElement => (
  <AppContexts>
    <Error>
      <AppError>
        <AppRouter />
      </AppError>
    </Error>
  </AppContexts>
);

export { App };
