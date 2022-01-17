import React from "react";
import Button from "react-bootstrap/Button";
import Col from "react-bootstrap/Col";
import Row from "react-bootstrap/Row";
import Form from "react-bootstrap/Form";
import { ChevronRight, Gear } from "react-bootstrap-icons";

import { EditorSettingsData, EditorType } from "~/contexts/EditorContext";

interface EditorSettingsProps {
  settings: EditorSettingsData;
}

const EditorSettings: React.FC<EditorSettingsProps> = (
  props
): React.ReactElement => {
  const { settings } = props;

  const onClickOpenSettings = React.useCallback((): void => {
    settings.setOpened(true);
  }, [settings]);
  const onClickCloseSettings = React.useCallback((): void => {
    settings.setOpened(false);
  }, [settings]);
  const onChangeEditor = React.useCallback(
    (e): void => {
      const editor = e.target.value as EditorType;
      if (editor === "vim") {
        //import(
        //  /* webpackChunkName: "codemirror-keymap-vim" */ "codemirror/keymap/vim"
        //).then((): void => {
        //  settings.setEditor(editor);
        //});
      } else if (editor === "emacs") {
        //import(
        //  /* webpackChunkName: "codemirror-keymap-emacs" */ "codemirror/keymap/emacs"
        //).then((): void => {
        //  settings.setEditor(editor);
        //});
      } else {
        settings.setEditor(editor);
      }
    },
    [settings]
  );
  const onChangeTabKey = React.useCallback(
    (e): void => {
      const tabKey = e.target.value;
      settings.setTabKey(tabKey);
    },
    [settings]
  );
  const onChangeTabWidth = React.useCallback(
    (e): void => {
      const tabWidth = e.target.value;
      settings.setTabWidth(tabWidth);
    },
    [settings]
  );
  const onChangeSmartIndent = React.useCallback(
    (e): void => {
      const smartIndent = e.target.checked;
      settings.setSmartIndent(smartIndent);
    },
    [settings]
  );

  //if (!settings.opened) {
  //  return (
  //    <Button variant="link" onClick={onClickOpenSettings}>
  //      <Gear />
  //    </Button>
  //  );
  //}

  return (
    <div className="d-flex flex-column align-items-stretch gap-8px">
      <Form.Group controlId="wb-keybinding">
        <Form.Label>Key Binding</Form.Label>
        <div className="px-8px">
          <Form.Control
            as="select"
            value={settings.editor}
            onChange={onChangeEditor}
          >
            <option value="default">default</option>
            <option value="vim">vim</option>
            <option value="emacs">emacs</option>
          </Form.Control>
        </div>
      </Form.Group>
      <Form.Group controlId="wb-tabkey">
        <Form.Label>TAB Key Inserted</Form.Label>
        <div className="px-8px">
          <Form.Control
            as="select"
            value={settings.tabKey}
            onChange={onChangeTabKey}
          >
            <option value="2">2 Spaces</option>
            <option value="4">4 Spaces</option>
            <option value="8">8 Spaces</option>
            <option value="tab">TAB</option>
          </Form.Control>
        </div>
      </Form.Group>
      <Form.Group controlId="wb-tabwidth">
        <Form.Label>TAB Width</Form.Label>
        <div className="px-8px">
          <Form.Control
            as="select"
            value={settings.tabWidth}
            onChange={onChangeTabWidth}
          >
            <option value="2">2</option>
            <option value="4">4</option>
            <option value="8">8</option>
          </Form.Control>
        </div>
      </Form.Group>
      <Form.Group controlId="wb-smartindent">
        <Form.Check
          type="checkbox"
          label="Smart Indent"
          checked={settings.smartIndent}
          onChange={onChangeSmartIndent}
        />
      </Form.Group>
    </div>
  );
};

export { EditorSettings };
