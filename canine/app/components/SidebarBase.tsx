import React, { useEffect, useState } from "react";
import { Button } from "react-bootstrap";
import { useSelector } from "react-redux";
import { AppState, useAppDispatch } from "~/store";
import { LayoutSidebarReverse, X } from "react-bootstrap-icons";
import { wandboxSlice, WandboxState } from "~/features/slice";
import { History } from "./Sidebar/History";
import { EditorSettings } from "./Sidebar/EditorSettings";

const SidebarBase: React.FC = () => {
  const { sidebarLocked, sidebarState } = useSelector(
    ({ wandbox: { sidebarLocked, sidebarState, history } }: AppState) => ({
      sidebarLocked,
      sidebarState,
      history,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  // sidebarState をそのまま利用すると、閉じる瞬間に中身が切り替わってしまうので、
  // 決して "none" にならない state を利用する
  const [state, setState] =
    useState<Omit<WandboxState["sidebarState"], "none">>("history");
  useEffect(() => {
    if (sidebarState === "none") {
      return;
    }
    setState(sidebarState);
  }, [sidebarState]);

  const title = state === "editorSettings" ? "Editor Settings" : "Log";
  const content = state === "editorSettings" ? <EditorSettings /> : <History />;

  return (
    <div className="d-flex flex-column">
      <div className="wb-nav d-flex justify-content-between px-8px py-4px">
        <div className="d-flex align-items-center gap-8px">
          <h5>{title}</h5>
          <Button
            variant={sidebarLocked ? "primary" : "link"}
            active={sidebarLocked}
            onClick={() => {
              dispatch(actions.setSidebarLocked(!sidebarLocked));
            }}
          >
            <LayoutSidebarReverse />
          </Button>
        </div>
        <Button
          variant="link"
          onClick={() => {
            dispatch(actions.setSidebarState("none"));
            dispatch(actions.setSidebarLocked(false));
          }}
        >
          <X />
        </Button>
      </div>
      {content}
    </div>
  );
};
export { SidebarBase };
