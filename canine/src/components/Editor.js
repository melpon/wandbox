// @flow
import React from 'react'
import Paper from '@material-ui/core/Paper'
import { withStyles } from '@material-ui/core/styles'
import EditorTabs from './EditorTabs'
import CodeMirror from './CodeMirror'
import { resolveLanguageMode } from '~/utils'
import type { Source as EditorSource } from '~/reducers/editor'

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
      />
    </Paper>
  )
}

export default withStyles(styles)(Editor)
