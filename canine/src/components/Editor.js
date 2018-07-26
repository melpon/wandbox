// @flow
import React from 'react'
import Paper from '@material-ui/core/Paper'
import { withStyles } from '@material-ui/core/styles'
import { Controlled as CodeMirror } from 'react-codemirror2'
import EditorTabs from './EditorTabs'
import { resolveLanguageMode } from '~/utils'
import type { Source as EditorSource } from '~/reducers/editor'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/material.css'
import 'codemirror/theme/material.css'

import 'codemirror/mode/clike/clike'
import 'codemirror/mode/d/d'
import 'codemirror/mode/ruby/ruby'
import 'codemirror/mode/python/python'
import 'codemirror/mode/perl/perl'
import 'codemirror/mode/erlang/erlang'
import 'codemirror/mode/haskell/haskell'
import 'codemirror/mode/shell/shell'
import 'codemirror/mode/lua/lua'
import 'codemirror/mode/php/php'
import 'codemirror/mode/commonlisp/commonlisp'
import 'codemirror/mode/pascal/pascal'
import 'codemirror/mode/rust/rust'
import 'codemirror/mode/groovy/groovy'
import 'codemirror/mode/javascript/javascript'
import 'codemirror/mode/coffeescript/coffeescript'
import 'codemirror/mode/swift/swift'
import 'codemirror/mode/mllike/mllike'
import 'codemirror/mode/go/go'
import 'codemirror-mode-elixir'

import 'codemirror/keymap/vim'
import 'codemirror/keymap/emacs'

import 'codemirror/addon/search/searchcursor'
import 'codemirror/addon/edit/matchbrackets'
import 'codemirror/addon/dialog/dialog'

const styles = _theme => ({})

type Props = {
  currentLanguage: string,
  currentTab: number,
  sources: Array<EditorSource>,
  onChangeEditorText: (string | null, string) => void,
  // below handlers are used in EditorTabs
  onChangeTabs: number => void,
  onClickTabEdit: EditorSource => void,
  onClickTabClose: EditorSource => void,
  onChangeRenamingFilename: (EditorSource, string) => void,
  onCancelRenamingFilename: EditorSource => void,
  onSubmitRenamingFilename: EditorSource => void
}

const Editor = (props: Props) => {
  const { currentLanguage, currentTab, onChangeEditorText, sources } = props
  const mode = resolveLanguageMode(
    sources[currentTab].filename,
    currentLanguage,
    'text/x-c++src'
  )
  console.log(mode)
  return (
    <Paper>
      <EditorTabs
        currentTab={props.currentTab}
        sources={props.sources}
        onChangeTabs={props.onChangeTabs}
        onClickTabEdit={props.onClickTabEdit}
        onClickTabClose={props.onClickTabClose}
        onChangeRenamingFilename={props.onChangeRenamingFilename}
        onCancelRenamingFilename={props.onCancelRenamingFilename}
        onSubmitRenamingFilename={props.onSubmitRenamingFilename}
      />
      <CodeMirror
        value={sources[currentTab].text}
        options={{
          lineNumbers: true,
          theme: 'material',
          mode: mode
        }}
        onBeforeChange={(_editor, _data, value) => {
          onChangeEditorText(sources[currentTab].filename, value)
        }}
        onChange={(_editor, _data, _value) => {}}
      />
    </Paper>
  )
}

export default withStyles(styles)(Editor)
