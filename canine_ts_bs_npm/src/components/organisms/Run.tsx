import React from "react";
import { useContainer } from "unstated-next";
import Button from "react-bootstrap/Button";

import { CompilerList } from "~/hooks/compilerList";
import { CompilerContext } from "~/contexts/CompilerContext";
import { EditorContext } from "~/contexts/EditorContext";
import { ResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";

export interface RunProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Run: React.FC<RunProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const editor = useContainer(EditorContext);
  const compiler = useContainer(CompilerContext);
  const result = useContainer(ResultContext);
  const { currentCompilerName } = compiler;
  const doCompile = useCompile(editor, compiler, compilerList, result);

  const onRun = React.useCallback((): void => {
    doCompile();
  }, [doCompile]);

  const disabled = permlinkData !== null || currentCompilerName === "";

  return (
    <Button onClick={onRun} disabled={disabled} variant="primary">
      Run
    </Button>
  );
};

export { Run };
