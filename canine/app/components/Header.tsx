import React, { useState } from "react";
import { useSelector } from "react-redux";
import Navbar from "react-bootstrap/Navbar";
import { Modal, Nav, NavDropdown } from "react-bootstrap";
import { AppState, useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  const { sidebarState, editorSettings } = useSelector(
    ({ wandbox: { sidebarState, editorSettings } }: AppState) => ({
      sidebarState,
      editorSettings,
    })
  );
  const { githubUser } = WANDBOX_LOADER_DATA;
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  return (
    <Navbar
      className="px-16px"
      bg="primary"
      variant="dark"
      style={{ height: 48 }}
    >
      <Navbar.Brand href="/">Wandbox</Navbar.Brand>
      <Navbar.Toggle />
      <Navbar.Collapse className="justify-content-end">
        <Nav>
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
            Settings
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
            Log
          </Nav.Link>
          <Nav.Link
            target="_blank"
            rel="noopener noreferrer"
            href="https://github.com/melpon/wandbox"
          >
            GitHub
          </Nav.Link>
          {githubUser === null ? (
            <Nav.Link
              href={`https://github.com/login/oauth/authorize?client_id=${WANDBOX_GITHUB_CLIENT_ID}`}
            >
              Login
            </Nav.Link>
          ) : (
            <NavDropdown
              title={
                <img
                  src={githubUser.avatar_url}
                  style={{ width: 24, height: 24, borderRadius: 12 }}
                />
              }
              align="end"
            >
              <NavDropdown.Item href="/logout">Logout</NavDropdown.Item>
            </NavDropdown>
          )}
        </Nav>
      </Navbar.Collapse>
    </Navbar>
  );
};

export { Header };
