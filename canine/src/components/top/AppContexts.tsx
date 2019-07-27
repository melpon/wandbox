import React from "react";

import { CompilerContext } from "~/contexts/CompilerContext";
import { EditorContext } from "~/contexts/EditorContext";

export const AppContexts: React.FC<{}> = ({ children }): React.ReactElement => {
  return (
    <CompilerContext.Provider>
      <EditorContext.Provider>{children}</EditorContext.Provider>
    </CompilerContext.Provider>
  );
};
