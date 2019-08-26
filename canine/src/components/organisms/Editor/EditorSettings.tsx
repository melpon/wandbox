import React from "react";
import List from "@material-ui/core/List";
import Divider from "@material-ui/core/Divider";
import IconButton from "@material-ui/core/IconButton";
import Select from "@material-ui/core/Select";
import InputLabel from "@material-ui/core/InputLabel";
import FormControl from "@material-ui/core/FormControl";
import FormControlLabel from "@material-ui/core/FormControlLabel";
import Checkbox from "@material-ui/core/Checkbox";
import SettingsIcon from "@material-ui/icons/SettingsRounded";
import ChevronRightIcon from "@material-ui/icons/ChevronRight";

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
        import(
          /* webpackChunkName: "codemirror-keymap-vim" */ "codemirror/keymap/vim"
        ).then((): void => {
          settings.setEditor(editor);
        });
      } else if (editor === "emacs") {
        import(
          /* webpackChunkName: "codemirror-keymap-emacs" */ "codemirror/keymap/emacs"
        ).then((): void => {
          settings.setEditor(editor);
        });
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

  if (!settings.opened) {
    return (
      <IconButton onClick={onClickOpenSettings}>
        <SettingsIcon />
      </IconButton>
    );
  }

  return (
    <List>
      <IconButton onClick={onClickCloseSettings}>
        <ChevronRightIcon />
      </IconButton>
      <Divider />
      <FormControl style={{ width: "100%" }}>
        <InputLabel>Key Binding</InputLabel>
        <Select native value={settings.editor} onChange={onChangeEditor}>
          <option value="default">default</option>
          <option value="vim">vim</option>
          <option value="emacs">emacs</option>
        </Select>
      </FormControl>
      <FormControl style={{ width: "100%" }}>
        <InputLabel>TAB Key Inserted</InputLabel>
        <Select native value={settings.tabKey} onChange={onChangeTabKey}>
          <option value="2">2 Spaces</option>
          <option value="4">4 Spaces</option>
          <option value="8">8 Spaces</option>
          <option value="tab">TAB</option>
        </Select>
      </FormControl>
      <FormControl style={{ width: "100%" }}>
        <InputLabel>TAB Width</InputLabel>
        <Select native value={settings.tabWidth} onChange={onChangeTabWidth}>
          <option value="2">2</option>
          <option value="4">4</option>
          <option value="8">8</option>
        </Select>
      </FormControl>
      <FormControlLabel
        control={
          <Checkbox
            checked={settings.smartIndent}
            onChange={onChangeSmartIndent}
            value="smartIndent"
          />
        }
        label="Smart Indent"
      />
    </List>
  );
};

export { EditorSettings };
