import React, { useState } from "react";
import Link from "next/link";
import Navbar from "react-bootstrap/Navbar";
import { Modal, Nav } from "react-bootstrap";
import { EditorSettings } from "./Editor/EditorSettings";
import { useEditorContext } from "~/contexts/EditorContext";

// eslint-disable-next-line @typescript-eslint/no-empty-interface
interface HeaderProps {}

const Header: React.FC<HeaderProps> = (): React.ReactElement => {
  const [showSettings, setShowSettings] = useState<boolean>(false);
  const editor = useEditorContext();

  return (
    <Navbar className="px-16px" bg="primary" variant="dark">
      <Navbar.Brand href="/">Wandbox</Navbar.Brand>
      <Navbar.Toggle />
      <Navbar.Collapse className="justify-content-end">
        <Nav>
          <Nav.Link onClick={() => setShowSettings(true)}>Settings</Nav.Link>
          <Nav.Link
            target="_blank"
            rel="noopener noreferrer"
            href="https://github.com/melpon/wandbox"
          >
            GitHub
          </Nav.Link>
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
          <EditorSettings settings={editor.settings} />
        </Modal.Body>
      </Modal>
    </Navbar>
  );
};

export { Header };
