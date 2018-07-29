// @flow
import React from 'react'
import { connect } from 'react-redux'
import CodeMirror from '~/components/CodeMirror'
import { changeEditorText } from '~/actions/editor'
import { resolveLanguageMode } from '~/utils'
import type { State as EditorState } from '~/reducers/editor'
import type { State as EditorSettingsState } from '~/reducers/editor/settings'
import type { CodeMirrorType, CodeMirrorOptions } from '~/components/CodeMirror'
import type { State as CompilerState } from '~/reducers/compiler'

type Props = {
  dispatch: Function,
  editor: EditorState,
  compiler: CompilerState
}
type State = {}

class Editor extends React.PureComponent<Props, State> {
  constructor() {
    super()
    this.onChangeEditorText = this.onChangeEditorText.bind(this)
  }

  onChangeEditorText: (string | null, string) => void
  onChangeEditorText(filename, text) {
    this.props.dispatch(changeEditorText(filename, text))
  }

  insertTabSpace(cm: CodeMirrorType) {
    const cursor = cm.getCursor()['ch']
    const indentUnit = cm.getOption('indentUnit')
    const newCursor =
      Math.floor((cursor + indentUnit) / indentUnit) * indentUnit
    const indentNum = newCursor - cursor
    const spaces = Array(indentNum + 1).join(' ')
    cm.replaceSelection(spaces, 'end', '+input')
  }

  getOptions(settings: EditorSettingsState): CodeMirrorOptions {
    let options = {
      keyMap: settings.editor,
      smartIndent: settings.smartIndent,
      tabSize: parseInt(settings.tabWidth, 10),
      extraKeys: {
        'Ctrl-Enter': () => {
          // TODO: compile
        }
      }
    }

    if (settings.tabKey == 'tab') {
      options = {
        ...options,
        extraKeys: {
          ...options.extraKeys,
          Tab: undefined
        },
        indentWithTabs: true
      }
    } else {
      options = {
        ...options,
        extraKeys: {
          ...options.extraKeys,
          Tab: this.insertTabSpace
        },
        indentUnit: parseInt(settings.tabKey, 10),
        indentWithTabs: true
      }
    }

    return options
  }
  render() {
    const { editor, compiler } = this.props
    const source = editor.sources[editor.currentTab]
    const settings = editor.settings

    const options = this.getOptions(settings)
    const mode = resolveLanguageMode(
      source.filename,
      compiler.currentLanguage,
      'text/x-text'
    )

    return (
      <CodeMirror
        expand={editor.settings.expand}
        value={source.text}
        options={{
          lineNumbers: true,
          theme: 'material',
          mode: mode,
          ...options
        }}
        onBeforeChange={(_editor, _data, value) => {
          this.onChangeEditorText(source.filename, value)
        }}
      />
    )
  }
}

function mapStateToProps(state) {
  return {
    editor: state.editor,
    compiler: state.compiler
  }
}

export default connect(mapStateToProps)(Editor)
