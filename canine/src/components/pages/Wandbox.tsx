import React, { useEffect } from "react";
import useReactRouter from "use-react-router";
import { makeStyles } from "@material-ui/styles";
import Grid from "@material-ui/core/Grid";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { useGetPermlink, PermlinkData } from "~/hooks/permlink";
import { Header } from "../organisms/Header";
import { Sidebar } from "../organisms/Sidebar";
import { Permlink } from "../organisms/Permlink";
import { Editor } from "../organisms/Editor";
import { Command } from "../organisms/Command";
import { Result } from "../organisms/Result";
import { Run } from "../organisms/Run";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const useStyles = makeStyles(() => ({
  sidebar: {
    paddingLeft: "15px",
    paddingTop: "15px"
  },
  contents: {
    paddingLeft: "15px",
    paddingTop: "15px"
  },
  permlink: {
    minHeight: "50px"
  }
}));

interface WandboxRouterProps {
  permlinkId?: string;
}

const Wandbox: React.FC<{}> = (): React.ReactElement | null => {
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
    <Grid container>
      <Grid item xs={12} sm={12}>
        <Header />
      </Grid>
      <Grid item xs={12} sm={12} container alignItems="flex-start">
        <Grid item xs={12} sm={2} className={classes.sidebar}>
          <Sidebar compilerList={compilerList} permlinkData={permlinkData} />
        </Grid>
        <Grid item xs={12} sm={10} className={classes.contents} container>
          <Grid item xs={12} sm={12} className={classes.permlink}>
            <Permlink
              compilerList={compilerList}
              permlinkData={permlinkData}
              clearPermlinkData={clearPermlinkData}
            />
          </Grid>
          <Grid item xs={12} sm={12}>
            <Editor compilerList={compilerList} permlinkData={permlinkData} />
          </Grid>
          <Grid item xs={12} sm={12}>
            <Grid container spacing={2}>
              <Grid item>
                <Run compilerList={compilerList} permlinkData={permlinkData} />
              </Grid>
              <Grid item>
                <Command
                  compilerList={compilerList}
                  permlinkData={permlinkData}
                />
              </Grid>
            </Grid>
          </Grid>
          <Grid item xs={12} sm={12}>
            <Result permlinkData={permlinkData} />
          </Grid>
        </Grid>
      </Grid>
    </Grid>
  );
};

export { Wandbox };
