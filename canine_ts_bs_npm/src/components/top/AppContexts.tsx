import React from "react";

import { CompilerContext } from "~/contexts/CompilerContext";
import { EditorContext } from "~/contexts/EditorContext";
import { ResultContext } from "~/contexts/ResultContext";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  return (
    <CompilerContext.Provider>
      <EditorContext.Provider>
        <ResultContext.Provider>{children}</ResultContext.Provider>
      </EditorContext.Provider>
    </CompilerContext.Provider>
  );
};

export { AppContexts };
