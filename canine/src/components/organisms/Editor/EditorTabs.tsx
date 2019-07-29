import React from "react";
//import { Theme } from "@material-ui/core/styles/createMuiTheme";
import { makeStyles } from "@material-ui/styles";
import Tabs from "@material-ui/core/Tabs";
import Tab from "@material-ui/core/Tab";
import AddBoxIcon from "@material-ui/icons/AddBox";
import { EditorContextState } from "~/contexts/EditorContext";
import { EditorTab, RenamingSource } from "./EditorTab";
import "codemirror/lib/codemirror.css";
import "codemirror/theme/material.css";

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const useStyles = makeStyles(() => ({
  tabRoot: {
    textTransform: "initial"
  }
}));

interface EditorTabsProps {
  editor: EditorContextState;
}

export const EditorTabs: React.FC<EditorTabsProps> = (
  props
): React.ReactElement | null => {
  const classes = useStyles();
  const { editor } = props;
  const [renamingSources, setRenamingSources] = React.useState<
    RenamingSource[]
  >([]);

  React.useEffect((): void => {
    const rs = editor.sources.map(
      (source): RenamingSource => ({
        renaming: false,
        // 0番目は source.filename == null になっていて、UI 上で編集不可なので、
        // 適当に空文字を入れておく
        filename: source.filename || "",
        originalFilename: source.filename || ""
      })
    );
    setRenamingSources(rs);
  }, [editor.sources]);

  const onChangeTabs = React.useCallback(
    (_e, index: number): void => {
      // 追加ボタン
      if (index === editor.sources.length) {
        editor.addSource("noname");
      } else {
        editor.setCurrentTab(index);
      }
    },
    [editor]
  );
  const onClickTabClose = React.useCallback(
    (index: number): void => {
      editor.removeSource(index);
    },
    [editor]
  );

  const onClickTabEdit = React.useCallback(
    (index: number): void => {
      const rs = [...renamingSources];
      rs[index] = { ...rs[index], renaming: true };
      setRenamingSources(rs);
    },
    [renamingSources]
  );
  const onChangeRenamingFilename = React.useCallback(
    (index: number, filename: string): void => {
      const rs = [...renamingSources];
      rs[index] = { ...rs[index], filename: filename };
      setRenamingSources(rs);
    },
    [renamingSources]
  );
  const onCancelRenamingFilename = React.useCallback(
    (index: number): void => {
      const rs = [...renamingSources];
      rs[index] = {
        ...rs[index],
        renaming: false,
        filename: rs[index].originalFilename
      };
      setRenamingSources(rs);
    },
    [renamingSources]
  );
  const onSubmitRenamingFilename = React.useCallback(
    (index: number): void => {
      editor.setFilename(index, renamingSources[index].filename);
      const rs = [...renamingSources];
      rs[index] = {
        ...rs[index],
        renaming: false,
        filename: rs[index].originalFilename
      };
      setRenamingSources(rs);
    },
    [renamingSources, editor]
  );

  return (
    <Tabs
      value={editor.currentTab}
      onChange={onChangeTabs}
      indicatorColor="primary"
      textColor="primary"
      scrollButtons="auto"
      variant="scrollable"
    >
      {editor.sources.map(
        (source, index): React.ReactElement => {
          return (
            <Tab
              key={index}
              classes={{ root: classes.tabRoot }}
              component={EditorTab}
              {...{
                index,
                source,
                renamingSource: renamingSources[index] || null,
                onClickTabEdit,
                onClickTabClose,
                onChangeRenamingFilename,
                onCancelRenamingFilename,
                onSubmitRenamingFilename
              }}
            />
          );
        }
      )}
      <Tab key={editor.sources.length} icon={<AddBoxIcon />} />
    </Tabs>
  );
};
