import React from "react";
import Nav from "react-bootstrap/Nav";
import { FileEarmarkPlus, Gear } from "react-bootstrap-icons";

import { PermlinkData } from "~/hooks/permlink";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { EditorTab, RenamingSource } from "./EditorTab";
import { Button } from "react-bootstrap";
import { wandboxSlice, WandboxState } from "~/features/slice";
import { useAppDispatch } from "~/store";
import { addSource } from "~/features/actions";

interface EditorTabsProps {
  state: WandboxState;
  permlinkData: PermlinkData | null;
}

const EditorTabs: React.FC<EditorTabsProps> = (
  props
): React.ReactElement | null => {
  const { state, permlinkData } = props;
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [renamingSources, setRenamingSources] = React.useState<
    RenamingSource[]
  >([]);

  // パーマリンク時は permlinkData からソースデータを作る
  const sources =
    permlinkData === null
      ? state.sources
      : createEditorSourceData(
          permlinkData.parameter.code,
          permlinkData.parameter.codes
        );

  React.useEffect((): void => {
    const rs = state.sources.map(
      (source): RenamingSource => ({
        renaming: false,
        // 0番目は source.filename == null になっていて、UI 上で編集不可なので、
        // 適当に空文字を入れておく
        filename: source.filename || "",
        originalFilename: source.filename || "",
      })
    );
    setRenamingSources(rs);
  }, [state.sources]);

  const onAddTab = React.useCallback((): void => {
    addSource(dispatch, state.sources, "noname");
  }, [state]);

  const onChangeTabs = React.useCallback(
    (index: number): void => {
      // 追加ボタン
      dispatch(actions.setCurrentTab(index));

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
    [state, renamingSources]
  );
  const onClickTabClose = React.useCallback((index: number): void => {
    dispatch(actions.removeSource(index));
  }, []);

  const onClickTabEdit = React.useCallback(
    (index: number): void => {
      dispatch(actions.setCurrentTab(index));
      const rs = renamingSources.map(
        (r, i): RenamingSource => ({
          ...r,
          renaming: i === index,
          filename: r.originalFilename,
        })
      );
      setRenamingSources(rs);
    },
    [state, renamingSources]
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
      dispatch(
        actions.setFilename({
          tab: index,
          filename: renamingSources[index].filename,
        })
      );
      const rs = [...renamingSources];
      rs[index] = {
        ...rs[index],
        renaming: false,
        filename: rs[index].originalFilename,
      };
      setRenamingSources(rs);
    },
    [renamingSources, state]
  );

  return (
    <div className="d-flex justify-content-between">
      <Nav
        className="wb-editortabs flex-grow-1"
        variant="tabs"
        activeKey={`wb-editor-${state.currentTab}`}
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
                active: index === state.currentTab,
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
      {!state.stdinOpened && (
        <Button
          variant="link"
          className="wb-stdinbutton align-self-end"
          onClick={() => dispatch(actions.setStdinOpened(!state.stdinOpened))}
        >
          Stdin
        </Button>
      )}
    </div>
  );
};

export { EditorTabs };
