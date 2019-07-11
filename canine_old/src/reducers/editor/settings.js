// @flow
import {
  OPEN_SETTINGS,
  CLOSE_SETTINGS,
  CHANGE_EDITOR,
  CHANGE_TAB_KEY,
  CHANGE_TAB_WIDTH,
  CHANGE_SMART_INDENT,
  CHANGE_EXPAND
} from '~/actions/editor/settings'

export type State = {
  opened: boolean,
  editor: 'default' | 'vim' | 'emacs',
  tabKey: '2' | '4' | '8' | 'tab',
  tabWidth: '2' | '4' | '8',
  smartIndent: boolean,
  expand: boolean
}
export const initialState = {
  opened: false,
  editor: 'default',
  tabKey: '4',
  tabWidth: '4',
  smartIndent: true,
  expand: false
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case OPEN_SETTINGS:
      return { ...state, opened: true }
    case CLOSE_SETTINGS:
      return { ...state, opened: false }
    case CHANGE_EDITOR:
      return { ...state, editor: action.editor }
    case CHANGE_TAB_KEY:
      return { ...state, tabKey: action.tabKey }
    case CHANGE_TAB_WIDTH:
      return { ...state, tabWidth: action.tabWidth }
    case CHANGE_SMART_INDENT:
      return { ...state, smartIndent: action.smartIndent }
    case CHANGE_EXPAND:
      return { ...state, expand: action.expand }
    default:
      return state
  }
}
