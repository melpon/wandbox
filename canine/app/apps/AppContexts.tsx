import React from "react";

import store from "~/store";
import { Provider } from "react-redux";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  return <Provider store={store}>{children}</Provider>;
};

export { AppContexts };
