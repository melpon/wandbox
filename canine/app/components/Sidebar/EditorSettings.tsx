import React from "react";
import { useSelector } from "react-redux";
import Form from "react-bootstrap/Form";
import { EditorType, wandboxSlice } from "~/features/slice";
import { AppState, useAppDispatch } from "~/store";

const EditorSettings: React.FC = () => {
  const settings = useSelector(
    ({ wandbox: { editorSettings } }: AppState) => editorSettings
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  const onChangeTabKey = React.useCallback(
    (e): void => {
      const tabKey = e.target.value;
      dispatch(actions.setEditorSettingsTabKey(tabKey));
    },
    [settings]
  );
  const onChangeTabWidth = React.useCallback(
    (e): void => {
      const tabWidth = e.target.value;
      dispatch(actions.setEditorSettingsTabWidth(tabWidth));
    },
    [settings]
  );
  const onChangeSmartIndent = React.useCallback(
    (e): void => {
      const smartIndent = e.target.checked;
      dispatch(actions.setEditorSettingsSmartIndent(smartIndent));
    },
    [settings]
  );

  return (
    <div className="d-flex flex-column align-items-stretch gap-8px">
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
            disabled={settings.tabKey !== "tab"}
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
