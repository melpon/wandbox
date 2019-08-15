import React, { useEffect } from "react";
import useReactRouter from "use-react-router";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { makeStyles } from "@material-ui/styles";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { Header } from "../organisms/Header";
import { Sidebar } from "../organisms/Sidebar";
import { Permlink } from "../organisms/Permlink";
import { Editor } from "../organisms/Editor";
import { Command } from "../organisms/Command";
import { Result } from "../organisms/Result";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const useStyles = makeStyles((theme: Theme) => ({
  root: theme.mixins.gutters({
    paddingTop: 16,
    paddingBottom: 16,
    marginTop: theme.spacing(3)
  })
}));

interface WandboxRouterProps {
  permlinkId?: string;
}

export const Wandbox: React.FC<{}> = (): React.ReactElement | null => {
  const { match } = useReactRouter<WandboxRouterProps>();
  const permlinkId =
    match.params.permlinkId === undefined ? null : match.params.permlinkId;
  const classes = useStyles();
  const [, setError] = useError();
  const compilerList = useCompilerList(
    "https://wandbox.org/api/list.json",
    setError
  );

  const [permlinkData, setPermlinkData] = React.useState<PermlinkData | null>(
    null
  );
  const [permlinkResp, , doGetPermlink] = useGetPermlink(
    permlinkId === null ? "" : permlinkId,
    setError
  );

  useEffect((): void => {
    if (permlinkId === null) {
      setPermlinkData(null);
      return;
    }

    doGetPermlink(`https://wandbox.org/api/permlink/${permlinkId}`, {});
  }, [permlinkId]);

  useEffect((): void => {
    if (permlinkResp === null) {
      return;
    }

    setPermlinkData(permlinkResp);
  }, [permlinkResp]);

  const clearPermlinkData = React.useCallback((): void => {
    setPermlinkData(null);
  }, [setPermlinkData]);

  if (compilerList === null) {
    return null;
  }

  return (
    <div className={classes.root}>
      <Header />
      <Sidebar compilerList={compilerList} permlinkData={permlinkData} />
      <Permlink
        compilerList={compilerList}
        permlinkData={permlinkData}
        clearPermlinkData={clearPermlinkData}
      />
      <Editor compilerList={compilerList} permlinkData={permlinkData} />
      <Command compilerList={compilerList} permlinkData={permlinkData} />
      <Result permlinkData={permlinkData} />
    </div>
  );
};
