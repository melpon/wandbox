// @flow
import type { Source as EditorSource } from '~/reducers/editor'

const makeType = name => `canine/editor/${name}`

export const CHANGE_EDITOR_TAB = makeType('CHANGE_EDITOR_TAB')
export function changeEditorTab(index: number) {
  return {
    type: CHANGE_EDITOR_TAB,
    tab: index
  }
}

export const CHANGE_EDITOR_TEXT = makeType('CHANGE_EDITOR_TEXT')
export function changeEditorText(filename: string | null, text: string) {
  return {
    type: CHANGE_EDITOR_TEXT,
    filename: filename,
    text: text
  }
}

export const ADD_EDITOR = makeType('ADD_EDITOR')
export function addEditor() {
  return {
    type: ADD_EDITOR
  }
}

export const BEGIN_RENAME = makeType('BEGIN_RENAME')
export function beginRename(source: EditorSource) {
  return {
    type: BEGIN_RENAME,
    source: source
  }
}

export const CHANGE_RENAME = makeType('CHANGE_RENAME')
export function changeRename(source: EditorSource, filename: string) {
  return {
    type: CHANGE_RENAME,
    source: source,
    filename: filename
  }
}
export const CANCEL_RENAME = makeType('CANCEL_RENAME')
export function cancelRename(source: EditorSource) {
  return {
    type: CANCEL_RENAME,
    source: source
  }
}
export const SUBMIT_RENAME = makeType('SUBMIT_RENAME')
export function submitRename(source: EditorSource) {
  return {
    type: SUBMIT_RENAME,
    source: source
  }
}

export const CLOSE_TAB = makeType('CLOSE_TAB')
export function closeTab(source: EditorSource) {
  return {
    type: CLOSE_TAB,
    source: source
  }
}
