import React from "react";
import { useSelector } from "react-redux";
import Navbar from "react-bootstrap/Navbar";
import { Nav, NavDropdown } from "react-bootstrap";
import { useTranslation } from "react-i18next";

import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  const { t, i18n } = useTranslation();
  const { sidebarState } = useSelector(
    ({ wandbox: { sidebarState } }: AppState) => ({
      sidebarState,
    })
  );
  const { githubUser } = WANDBOX_LOADER_DATA;
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  return (
    <Navbar
      className="px-16px justify-content-between"
      bg="dark"
      variant="dark"
      style={{ padding: "5px 16px", zIndex: 10 }}
      expand="md"
    >
      <Navbar.Brand href="/">Wandbox</Navbar.Brand>
      {/* スマホ表示用 */}
      <Nav className="d-flex d-md-none flex-row gap-8px gap-md-0px">
        <Nav.Link
          active={sidebarState === "editorSettings"}
          onClick={() => {
            if (sidebarState === "editorSettings") {
              dispatch(actions.setSidebarState("none"));
              dispatch(actions.setSidebarLocked(false));
            } else {
              dispatch(actions.setSidebarState("editorSettings"));
            }
          }}
        >
          {t("header.settings")}
        </Nav.Link>
        <Nav.Link
          active={sidebarState === "history"}
          onClick={() => {
            if (sidebarState === "history") {
              dispatch(actions.setSidebarState("none"));
              dispatch(actions.setSidebarLocked(false));
            } else {
              dispatch(actions.setSidebarState("history"));
            }
          }}
        >
          {t("header.history")}
        </Nav.Link>
        <Navbar.Toggle aria-controls="wb-navbar" />
      </Nav>

      <Navbar.Collapse id="wb-navbar" className="justify-content-end">
        <Nav className="d-none d-md-flex">
          <Nav.Link
            active={sidebarState === "editorSettings"}
            onClick={() => {
              if (sidebarState === "editorSettings") {
                dispatch(actions.setSidebarState("none"));
                dispatch(actions.setSidebarLocked(false));
              } else {
                dispatch(actions.setSidebarState("editorSettings"));
              }
            }}
          >
            {t("header.settings")}
          </Nav.Link>
          <Nav.Link
            active={sidebarState === "history"}
            onClick={() => {
              if (sidebarState === "history") {
                dispatch(actions.setSidebarState("none"));
                dispatch(actions.setSidebarLocked(false));
              } else {
                dispatch(actions.setSidebarState("history"));
              }
            }}
          >
            {t("header.history")}
          </Nav.Link>
        </Nav>
        <Nav className="align-items-md-center">
          <NavDropdown title={t("header.language")} align="end">
            <NavDropdown.Item
              onClick={() => {
                i18n.changeLanguage("en-US");
              }}
            >
              {t("header.languageEn")}
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() => {
                i18n.changeLanguage("ja-jp");
              }}
            >
              {t("header.languageJa")}
            </NavDropdown.Item>
          </NavDropdown>
          <Nav.Link
            target="_blank"
            rel="noopener noreferrer"
            href="https://github.com/melpon/wandbox"
          >
            {t("header.github")}
          </Nav.Link>
          {githubUser === null ? (
            <Nav.Link
              href={`https://github.com/login/oauth/authorize?client_id=${WANDBOX_GITHUB_CLIENT_ID}`}
            >
              {t("header.login")}
            </Nav.Link>
          ) : (
            <NavDropdown
              className="wb-nav-icondropdown"
              title={
                <img
                  src={githubUser.avatar_url}
                  style={{ width: 24, height: 24, borderRadius: 12 }}
                />
              }
              align="end"
            >
              <NavDropdown.Item href="/logout">
                {t("header.logout")}
              </NavDropdown.Item>
            </NavDropdown>
          )}
        </Nav>
      </Navbar.Collapse>
    </Navbar>
  );
};

export { Header };
