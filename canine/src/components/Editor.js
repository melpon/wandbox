// @flow
import React from 'react'
import Paper from '@material-ui/core/Paper'
import { withStyles } from '@material-ui/core/styles'
import { Controlled as CodeMirror } from 'react-codemirror2'
import EditorTabs from './EditorTabs'
import type { Source as EditorSource } from '~/reducers/editor'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/material.css'

const styles = _theme => ({})

type Props = {
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
  const { currentTab, onChangeEditorText, sources } = props
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
          lineNumbers: true
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
