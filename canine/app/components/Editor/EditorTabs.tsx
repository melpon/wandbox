import React, { useCallback, useEffect, useState } from "react";
import Nav from "react-bootstrap/Nav";
import { FileEarmarkPlus } from "react-bootstrap-icons";
import { Button } from "react-bootstrap";
import { useTranslation } from "react-i18next";

import type { PermlinkData } from "~/hooks/permlink";
import type { RenamingSource } from "./EditorTab";
import { EditorTab } from "./EditorTab";
import type { EditorSourceData } from "~/features/slice";
import { wandboxSlice } from "~/features/slice";
import { useAppDispatch } from "~/store";
import { addSource } from "~/features/actions";

interface EditorTabsProps {
  currentTab: number;
  stdinOpened: boolean;
  sources: EditorSourceData[];
  permlinkData: PermlinkData | null;
}

const EditorTabs: React.FC<EditorTabsProps> = (
  props
): React.ReactElement | null => {
  const { currentTab, stdinOpened, sources, permlinkData } = props;
  const { t } = useTranslation();
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [renamingSources, setRenamingSources] = useState<RenamingSource[]>([]);

  useEffect((): void => {
    const rs = sources.map(
      (source): RenamingSource => ({
        renaming: false,
        // 0番目は source.filename == null になっていて、UI 上で編集不可なので、
        // 適当に空文字を入れておく
        filename: source.filename || "",
        originalFilename: source.filename || "",
      })
    );
    setRenamingSources(rs);
  }, [sources]);

  const onAddTab = useCallback((): void => {
    addSource(dispatch, sources, "noname");
  }, [sources]);

  const onChangeTabs = useCallback(
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
    [renamingSources]
  );
  const onClickTabClose = useCallback((index: number): void => {
    dispatch(actions.removeSource(index));
  }, []);

  const onClickTabEdit = useCallback(
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
    [renamingSources]
  );
  const onChangeRenamingFilename = useCallback(
    (index: number, filename: string): void => {
      const rs = [...renamingSources];
      rs[index] = { ...rs[index], filename: filename };
      setRenamingSources(rs);
    },
    [renamingSources]
  );
  const onCancelRenamingFilename = useCallback(
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
  const onSubmitRenamingFilename = useCallback(
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
    [renamingSources]
  );

  return (
    <div className="d-flex justify-content-between">
      <Nav
        className="wb-editortabs flex-grow-1"
        variant="tabs"
        activeKey={`wb-editor-${currentTab}`}
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
                active: index === currentTab,
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
      </Nav>

      {!stdinOpened && (
        <Button
          variant="link"
          className="wb-stdinbutton d-none d-md-block align-self-end"
          onClick={() => dispatch(actions.setStdinOpened(!stdinOpened))}
        >
          {t("editor.stdinTab")}
        </Button>
      )}
    </div>
  );
};

export { EditorTabs };
