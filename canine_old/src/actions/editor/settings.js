// @flow
const makeType = name => `canine/editor/settings/${name}`

export const OPEN_SETTINGS = makeType('OPEN_SETTINGS')
export function openSettings() {
  return { type: OPEN_SETTINGS }
}

export const CLOSE_SETTINGS = makeType('CLOSE_SETTINGS')
export function closeSettings() {
  return { type: CLOSE_SETTINGS }
}

export const CHANGE_EDITOR = makeType('CHANGE_EDITOR')
export function changeEditor(editor: string) {
  return {
    type: CHANGE_EDITOR,
    editor: editor
  }
}

export const CHANGE_TAB_KEY = makeType('CHANGE_TAB_KEY')
export function changeTabKey(tabKey: string) {
  return {
    type: CHANGE_TAB_KEY,
    tabKey: tabKey
  }
}

export const CHANGE_TAB_WIDTH = makeType('CHANGE_TAB_WIDTH')
export function changeTabWidth(tabWidth: string) {
  return {
    type: CHANGE_TAB_WIDTH,
    tabWidth: tabWidth
  }
}

export const CHANGE_SMART_INDENT = makeType('CHANGE_SMART_INDENT')
export function changeSmartIndent(smartIndent: boolean) {
  return {
    type: CHANGE_SMART_INDENT,
    smartIndent: smartIndent
  }
}

export const CHANGE_EXPAND = makeType('CHANGE_EXPAND')
export function changeExpand(expand: boolean) {
  return {
    type: CHANGE_EXPAND,
    expand: expand
  }
}
