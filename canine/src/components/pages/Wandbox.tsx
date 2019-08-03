import React from "react";
import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { makeStyles } from "@material-ui/styles";

import { useCompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
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

export interface WandboxProps {
  permlinkId?: string;
}

export const Wandbox: React.FC<WandboxProps> = (
  props
): React.ReactElement | null => {
  const permlinkId = props.permlinkId === undefined ? null : props.permlinkId;
  const classes = useStyles();
  const [, setError] = useError();
  const compilerList = useCompilerList(
    "https://wandbox.org/api/list.json",
    setError
  );

  if (compilerList === null) {
    return null;
  }
  console.log(compilerList);

  return (
    <div className={classes.root}>
      <Header />
      <Sidebar compilerList={compilerList} />
      <Permlink compilerList={compilerList} permlinkId={permlinkId} />
      <Editor compilerList={compilerList} />
      <Command compilerList={compilerList} />
      <Result />
    </div>
  );
};
