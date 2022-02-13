import React from "react";

import { EditorContext, useEditorContextState } from "~/contexts/EditorContext";
import {
  SidebarContext,
  useSidebarContextState,
} from "~/contexts/SidebarContext";
import store from "~/store";
import { Provider } from "react-redux";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  const editorContext = useEditorContextState();
  const sidebarContext = useSidebarContextState();
  return (
    <Provider store={store}>
      <EditorContext.Provider value={editorContext}>
        <SidebarContext.Provider value={sidebarContext}>
          {children}
        </SidebarContext.Provider>
      </EditorContext.Provider>
    </Provider>
  );
};

export { AppContexts };
