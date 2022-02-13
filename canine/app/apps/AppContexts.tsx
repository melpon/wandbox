import React from "react";

import {
  SidebarContext,
  useSidebarContextState,
} from "~/contexts/SidebarContext";
import store from "~/store";
import { Provider } from "react-redux";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  const sidebarContext = useSidebarContextState();
  return (
    <Provider store={store}>
      <SidebarContext.Provider value={sidebarContext}>
        {children}
      </SidebarContext.Provider>
    </Provider>
  );
};

export { AppContexts };
