import React from "react";

import { CompilerContext, useCompilerContextState } from "~/contexts/CompilerContext";
import { EditorContext, useEditorContextState } from "~/contexts/EditorContext";
import { ResultContext, useResultContextState } from "~/contexts/ResultContext";

const AppContexts: React.FC = ({ children }): React.ReactElement => {
  const compilerContext = useCompilerContextState();
  const editorContext = useEditorContextState();
  const resultContext = useResultContextState();
  return (
    <CompilerContext.Provider value={compilerContext}>
      <EditorContext.Provider value={editorContext}>
        <ResultContext.Provider value={resultContext}>{children}</ResultContext.Provider>
      </EditorContext.Provider>
    </CompilerContext.Provider>
  );
};

export { AppContexts };
