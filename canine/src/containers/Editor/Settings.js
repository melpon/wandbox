// @flow
import React from 'react'
import { connect } from 'react-redux'
import List from '@material-ui/core/List'
import Divider from '@material-ui/core/Divider'
import IconButton from '@material-ui/core/IconButton'
import Select from '@material-ui/core/Select'
import InputLabel from '@material-ui/core/InputLabel'
import FormControl from '@material-ui/core/FormControl'
import FormControlLabel from '@material-ui/core/FormControlLabel'
import Checkbox from '@material-ui/core/Checkbox'
import SettingsIcon from '@material-ui/icons/Settings'
import ChevronRightIcon from '@material-ui/icons/ChevronRight'
import {
  openSettings,
  closeSettings,
  changeEditor,
  changeTabKey,
  changeTabWidth,
  changeSmartIndent,
  changeExpand
} from '~/actions/editor/settings'
import type { State as EditorSettingsState } from '~/reducers/editor/settings'

type Props = {
  dispatch: Function,
  settings: EditorSettingsState
}
type State = {}

class Settings extends React.PureComponent<Props, State> {
  constructor() {
    super()
  }

  onClickOpenSettings() {
    this.props.dispatch(openSettings())
  }

  onClickCloseSettings() {
    this.props.dispatch(closeSettings())
  }

  onChangeEditor(editor: string) {
    this.props.dispatch(changeEditor(editor))
  }

  onChangeTabKey(tabKey: string) {
    this.props.dispatch(changeTabKey(tabKey))
  }

  onChangeTabWidth(tabWidth: string) {
    this.props.dispatch(changeTabWidth(tabWidth))
  }

  onChangeSmartIndent(smartIndent: boolean) {
    this.props.dispatch(changeSmartIndent(smartIndent))
  }

  onChangeExpand(expand: boolean) {
    this.props.dispatch(changeExpand(expand))
  }

  render() {
    const settings = this.props.settings
    if (settings.opened) {
      return (
        <List>
          <IconButton onClick={() => this.onClickCloseSettings()}>
            <ChevronRightIcon />
          </IconButton>
          <Divider />
          <FormControl style={{ width: '100%' }}>
            <InputLabel>Key Binding</InputLabel>
            <Select
              native
              value={settings.editor}
              onChange={e => this.onChangeEditor(e.target.value)}
            >
              <option value="default">default</option>
              <option value="vim">vim</option>
              <option value="emacs">emacs</option>
            </Select>
          </FormControl>
          <FormControl style={{ width: '100%' }}>
            <InputLabel>TAB Key Inserted</InputLabel>
            <Select
              native
              value={settings.tabKey}
              onChange={e => this.onChangeTabKey(e.target.value)}
            >
              <option value="2">2 Spaces</option>
              <option value="4">4 Spaces</option>
              <option value="8">8 Spaces</option>
              <option value="tab">TAB</option>
            </Select>
          </FormControl>
          <FormControl style={{ width: '100%' }}>
            <InputLabel>TAB Width</InputLabel>
            <Select
              native
              value={settings.tabWidth}
              onChange={e => this.onChangeTabWidth(e.target.value)}
            >
              <option value="2">2</option>
              <option value="4">4</option>
              <option value="8">8</option>
            </Select>
          </FormControl>
          <FormControlLabel
            control={
              <Checkbox
                checked={settings.smartIndent}
                onChange={e => this.onChangeSmartIndent(e.target.checked)}
                value="smartIndent"
              />
            }
            label="Smart Indent"
          />
          <FormControlLabel
            control={
              <Checkbox
                checked={settings.expand}
                onChange={e => this.onChangeExpand(e.target.checked)}
                value="expand"
              />
            }
            label="Expand"
          />
        </List>
      )
    } else {
      return (
        <IconButton onClick={() => this.onClickOpenSettings()}>
          <SettingsIcon />
        </IconButton>
      )
    }
  }
}

function mapStateToProps(state) {
  return {
    settings: state.editor.settings
  }
}

export default connect(mapStateToProps)(Settings)
