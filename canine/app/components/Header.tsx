import React, { useState } from "react";
import { useSelector } from "react-redux";
import Navbar from "react-bootstrap/Navbar";
import { Modal, Nav, NavDropdown } from "react-bootstrap";
import { EditorSettings } from "./Editor/EditorSettings";
import { AppState, useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  const [showSettings, setShowSettings] = useState<boolean>(false);
  const { historyOpened, editorSettings } = useSelector(
    ({ wandbox: { historyOpened, editorSettings } }: AppState) => ({
      historyOpened,
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
          <NavDropdown title="Tools" align="end">
            <NavDropdown.Item onClick={() => setShowSettings(true)}>
              Editor Settings
            </NavDropdown.Item>
            <NavDropdown.Item
              onClick={() => {
                if (historyOpened) {
                  dispatch(actions.setHistoryOpened(false));
                  dispatch(actions.setHistoryLocked(false));
                } else {
                  dispatch(actions.setHistoryOpened(true));
                }
              }}
            >
              History
            </NavDropdown.Item>
          </NavDropdown>
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
      <Modal show={showSettings} onHide={() => setShowSettings(false)}>
        <Modal.Body className="d-flex flex-column">
          <button
            type="button"
            className="align-self-end btn-close"
            data-bs-dismiss="modal"
            aria-label="Close"
            onClick={() => setShowSettings(false)}
          />
          <EditorSettings settings={editorSettings} />
        </Modal.Body>
      </Modal>
    </Navbar>
  );
};

export { Header };
