import React from "react";
import Button from "react-bootstrap/Button";

import { CompilerList } from "~/hooks/compilerList";
import {
  CompilerContext,
  useCompilerContext,
} from "~/contexts/CompilerContext";
import { EditorContext, useEditorContext } from "~/contexts/EditorContext";
import { ResultContext, useResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";

export interface RunProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Run: React.FC<RunProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const editor = useEditorContext();
  const compiler = useCompilerContext();
  const result = useResultContext();
  const { currentCompilerName } = compiler;
  const doCompile = useCompile(editor, compiler, compilerList, result);
  console.log(compiler);

  const onRun = React.useCallback((): void => {
    doCompile();
  }, [doCompile]);

  const disabled = permlinkData !== null || currentCompilerName === "";

  return (
    <Button
      className="align-self-start"
      style={{ minWidth: 144 }}
      onClick={onRun}
      disabled={disabled}
      variant="primary"
    >
      Run
    </Button>
  );
};

export { Run };
