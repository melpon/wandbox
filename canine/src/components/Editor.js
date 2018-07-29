// @flow
import React from 'react'
import Paper from '@material-ui/core/Paper'
import List from '@material-ui/core/List'
import Grid from '@material-ui/core/Grid'
import Typography from '@material-ui/core/Typography'
import Divider from '@material-ui/core/Divider'
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
      <Grid container>
        <Grid item style={{ flex: 1 }}>
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
            expand={false}
          />
        </Grid>
        <Grid item style={{ width: 200 }}>
          <List>
            <Typography>test</Typography>
            <Divider />
            <Typography>test2</Typography>
          </List>
        </Grid>
      </Grid>
    </Paper>
  )
}

export default withStyles(styles)(Editor)
