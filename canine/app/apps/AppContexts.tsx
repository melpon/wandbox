import React from "react";

import { EditorContext, useEditorContextState } from "~/contexts/EditorContext";
import { ResultContext, useResultContextState } from "~/contexts/ResultContext";
import {
  SidebarContext,
  useSidebarContextState,
} from "~/contexts/SidebarContext";
import store from "~/store";
import { Provider } from "react-redux";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  const editorContext = useEditorContextState();
  const resultContext = useResultContextState();
  const sidebarContext = useSidebarContextState();
  return (
    <Provider store={store}>
      <EditorContext.Provider value={editorContext}>
        <ResultContext.Provider value={resultContext}>
          <SidebarContext.Provider value={sidebarContext}>
            {children}
          </SidebarContext.Provider>
        </ResultContext.Provider>
      </EditorContext.Provider>
    </Provider>
  );
};

export { AppContexts };
