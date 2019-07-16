import React from "react";
import { BrowserRouter, Switch, Route } from "react-router-dom";
import { Wandbox } from "../pages/Wandbox";

export const AppRouter: React.FC<{}> = (): React.ReactElement => {
  return (
    <BrowserRouter>
      <Switch>
        <Route path="/" exact component={Wandbox} />
      </Switch>
    </BrowserRouter>
  );
};
