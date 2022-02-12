import React from "react";
import Nav from "react-bootstrap/Nav";
import { FileEarmarkPlus, Gear } from "react-bootstrap-icons";

import { EditorContextState } from "~/contexts/EditorContext";
import { PermlinkData } from "~/hooks/permlink";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { EditorTab, RenamingSource } from "./EditorTab";
import { Button } from "react-bootstrap";

interface EditorTabsProps {
  editor: EditorContextState;
  permlinkData: PermlinkData | null;
}

const EditorTabs: React.FC<EditorTabsProps> = (
  props
): React.ReactElement | null => {
  const { editor, permlinkData } = props;
  const [renamingSources, setRenamingSources] = React.useState<
    RenamingSource[]
  >([]);

  // パーマリンク時は permlinkData からソースデータを作る
  const sources =
    permlinkData === null
      ? editor.sources
      : createEditorSourceData(
          permlinkData.parameter.code,
          permlinkData.parameter.codes
        );

  React.useEffect((): void => {
    const rs = editor.sources.map(
      (source): RenamingSource => ({
        renaming: false,
        // 0番目は source.filename == null になっていて、UI 上で編集不可なので、
        // 適当に空文字を入れておく
        filename: source.filename || "",
        originalFilename: source.filename || "",
      })
    );
    setRenamingSources(rs);
  }, [editor.sources]);

  const onAddTab = React.useCallback((): void => {
    editor.addSource("noname");
  }, [editor]);

  const onChangeTabs = React.useCallback(
    (index: number): void => {
      // 追加ボタン
      editor.setCurrentTab(index);

      // 編集中のタブはキャンセルする
      const rs = renamingSources.map(
        (r): RenamingSource => ({
          ...r,
          renaming: false,
          filename: r.originalFilename,
        })
      );

      setRenamingSources(rs);
    },
    [editor, renamingSources]
  );
  const onClickTabClose = React.useCallback(
    (index: number): void => {
      editor.removeSource(index);
    },
    [editor]
  );

  const onClickTabEdit = React.useCallback(
    (index: number): void => {
      editor.setCurrentTab(index);
      const rs = renamingSources.map(
        (r, i): RenamingSource => ({
          ...r,
          renaming: i === index,
          filename: r.originalFilename,
        })
      );
      setRenamingSources(rs);
    },
    [editor, renamingSources]
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
        filename: rs[index].originalFilename,
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
        filename: rs[index].originalFilename,
      };
      setRenamingSources(rs);
    },
    [renamingSources, editor]
  );

  return (
    <div className="d-flex justify-content-between">
      <Nav
        className="wb-editortabs flex-grow-1"
        variant="tabs"
        activeKey={`wb-editor-${editor.currentTab}`}
      >
        {sources.map((source, index): React.ReactElement => {
          return (
            <EditorTab
              key={index}
              {...{
                index,
                source,
                readonly: permlinkData !== null,
                renamingSource: renamingSources[index] || null,
                active: index === editor.currentTab,
                onChangeTabs,
                onClickTabEdit,
                onClickTabClose,
                onChangeRenamingFilename,
                onCancelRenamingFilename,
                onSubmitRenamingFilename,
              }}
            />
          );
        })}

        {/* Permlink の場合はタブの追加不可 */}
        {permlinkData === null ? (
          <Nav.Item>
            <Nav.Link onClick={onAddTab}>
              <FileEarmarkPlus />
            </Nav.Link>
          </Nav.Item>
        ) : null}
        {/*sources.map(
        (source, index): React.ReactElement => {
          return (
            <Tab
              key={index}
              disableRipple
              classes={{ root: classes.tabRoot }}
              component={EditorTab}
              {...{
                index,
                source,
                readonly: permlinkData !== null,
                renamingSource: renamingSources[index] || null,
                onClickTabEdit,
                onClickTabClose,
                onChangeRenamingFilename,
                onCancelRenamingFilename,
                onSubmitRenamingFilename,
              }}
            />
          );
        }
      )*/}
      </Nav>
      {!editor.stdinOpened && (
        <Button
          variant="link"
          className="wb-stdinbutton align-self-end"
          onClick={() => editor.setStdinOpened(!editor.stdinOpened)}
        >
          Stdin
        </Button>
      )}
    </div>
  );
};

export { EditorTabs };
