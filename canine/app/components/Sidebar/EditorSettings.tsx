import React, { useCallback } from "react";
import { useSelector } from "react-redux";
import Form from "react-bootstrap/Form";
import { useTranslation } from "react-i18next";

import { wandboxSlice } from "~/features/slice";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";

const EditorSettings: React.FC = () => {
  const { t } = useTranslation();
  const settings = useSelector(
    ({ wandbox: { editorSettings } }: AppState) => editorSettings
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  const onChangeTabKey = useCallback(
    (e): void => {
      const tabKey = e.target.value;
      dispatch(actions.setEditorSettingsTabKey(tabKey));
    },
    [settings]
  );
  const onChangeTabWidth = useCallback(
    (e): void => {
      const tabWidth = e.target.value;
      dispatch(actions.setEditorSettingsTabWidth(tabWidth));
    },
    [settings]
  );
  const onChangeFixedHeight = useCallback(
    (e): void => {
      const fixedHeight = e.target.checked;
      dispatch(actions.setEditorSettingsFixedHeight(fixedHeight));
    },
    [settings]
  );

  return (
    <div className="d-flex flex-column align-items-stretch gap-8px">
      <Form.Group controlId="wb-tabkey">
        <Form.Label>{t("settings.tabKeyInserted")}</Form.Label>
        <div className="px-8px">
          <Form.Control
            as="select"
            value={settings.tabKey}
            onChange={onChangeTabKey}
          >
            <option value="2">{t("settings.tabKeyInserted2Spaces")}</option>
            <option value="4">{t("settings.tabKeyInserted4Spaces")}</option>
            <option value="8">{t("settings.tabKeyInserted8Spaces")}</option>
            <option value="tab">{t("settings.tabKeyInsertedTab")}</option>
          </Form.Control>
        </div>
      </Form.Group>
      <Form.Group controlId="wb-tabwidth">
        <Form.Label>{t("settings.tabWidth")}</Form.Label>
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
      <Form.Group controlId="wb-fixedheight">
        <Form.Check
          type="checkbox"
          label={t("settings.fixEditorHeight")}
          checked={settings.fixedHeight}
          onChange={onChangeFixedHeight}
        />
      </Form.Group>
    </div>
  );
};

export { EditorSettings };
