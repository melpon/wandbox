// @flow
const makeType = name => `canine/editor/${name}`

export const CHANGE_EDITOR_TEXT = makeType('CHANGE_EDITOR_TEXT')
export function changeEditorText(filename: string | null, text: string) {
  return {
    type: CHANGE_EDITOR_TEXT,
    filename: filename,
    text: text
  }
}

export const CHANGE_STDIN = makeType('CHANGE_STDIN')
export function changeStdin(stdin: string) {
  return {
    type: CHANGE_STDIN,
    stdin: stdin
  }
}
