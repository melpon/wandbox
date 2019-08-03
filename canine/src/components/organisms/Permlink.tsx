import React, { useEffect } from "react";
import { useContainer } from "unstated-next";
import useReactRouter from "use-react-router";
import Button from "@material-ui/core/Button";

import { ResultContext } from "~/contexts/ResultContext";
import { EditorContext } from "~/contexts/EditorContext";
import { CompilerContext } from "~/contexts/CompilerContext";
import { useFetchJSON, AnyJson, JsonMap } from "~/hooks/fetch";
import { createBody } from "~/utils/compile";
import { CompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";

export interface PermlinkData {
  permlink: string;
}

function resolvePermlink(json: AnyJson): PermlinkData {
  const map = json as JsonMap;
  return {
    permlink: map.permlink as string
  };
}

export interface PermlinkProps {
  compilerList: CompilerList;
  permlinkId: string | null;
}

export const Permlink: React.FC<PermlinkProps> = (
  props
): React.ReactElement | null => {
  const { compilerList, permlinkId } = props;
  const { history } = useReactRouter();
  const editor = useContainer(EditorContext);
  const compiler = useContainer(CompilerContext);
  const result = useContainer(ResultContext);
  const [, setError] = useError();
  const [sharing, setSharing] = React.useState<boolean>(false);

  const headers = {
    "Content-Type": "application/json"
  };
  const [permlinkResp, permlinkFetchId, doPermlink] = useFetchJSON<
    PermlinkData
  >(
    "https://wandbox.org/api/permlink",
    { method: "POST", headers: headers },
    resolvePermlink,
    setError
  );
  const onShare = React.useCallback((): void => {
    setSharing(true);
    const json = createBody(editor, compiler, compilerList);
    if (json === null) {
      return;
    }
    const body = JSON.stringify({
      ...json,
      results: result.results,
      login: false
    });

    doPermlink(null, { body: body });
  }, [compilerList, editor, compiler]);

  useEffect((): void => {
    // 初回での更新は弾く
    if (permlinkResp === null) {
      return;
    }

    // リクエストが完了したら共有ボタンを有効にする
    setSharing(false);

    // URL の切り替え
    history.push(`/permlink/${permlinkResp.permlink}`);
  }, [permlinkFetchId]);

  const onEdit = React.useCallback((): void => {}, []);

  if (result.results.length === 0) {
    return null;
  }

  if (permlinkId === null) {
    return <Button onClick={onShare}>Share</Button>;
  } else {
    return (
      <Button onClick={onEdit} disabled={sharing}>
        Edit
      </Button>
    );
  }
};
