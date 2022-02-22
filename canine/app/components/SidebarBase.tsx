import React, { useEffect, useState } from "react";
import { Button } from "react-bootstrap";
import { useSelector } from "react-redux";
import { LayoutSidebarReverse, X } from "react-bootstrap-icons";
import { useTranslation } from "react-i18next";

import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import type { Breakpoint, WandboxState } from "~/features/slice";
import { wandboxSlice } from "~/features/slice";
import { History } from "./Sidebar/History";
import { EditorSettings } from "./Sidebar/EditorSettings";

const SidebarBase: React.FC = () => {
  const { t } = useTranslation();
  const { sidebarLocked, sidebarState, breakpoint } = useSelector(
    ({ wandbox: { sidebarLocked, sidebarState, breakpoint } }: AppState) => ({
      sidebarLocked,
      sidebarState,
      breakpoint,
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

  const [currentBreakpoint, setCurrentBreakpoint] =
    useState<Breakpoint>(breakpoint);

  useEffect(() => {
    // md -> sm に変化した
    if (
      (currentBreakpoint === "xxl" ||
        currentBreakpoint === "xl" ||
        currentBreakpoint === "lg" ||
        currentBreakpoint === "md") &&
      (breakpoint === "sm" || breakpoint === "xs")
    ) {
      // sm 以下のサイズではサイドバーのロックが使えないので
      // サイドバーを強制的に閉じてアンロックする
      dispatch(actions.setSidebarState("none"));
      dispatch(actions.setSidebarLocked(false));
    }
    setCurrentBreakpoint(breakpoint);
  }, [breakpoint]);

  const title =
    state === "editorSettings" ? t("settings.title") : t("history.title");
  const content = state === "editorSettings" ? <EditorSettings /> : <History />;

  return (
    <div className="d-flex flex-column">
      <div className="wb-nav d-flex justify-content-between px-8px py-4px">
        <div className="d-flex align-items-center gap-8px">
          <h5>{title}</h5>
          <Button
            className="d-none d-md-block"
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
