// @flow
import React from 'react'
import { connect } from 'react-redux'
import Paper from '@material-ui/core/Paper'
import Grid from '@material-ui/core/Grid'
import EditorTabs from './Editor/Tabs.js'
import EditorEditor from './Editor/Editor.js'
import EditorSettings from './Editor/Settings.js'
import type { State as EditorSettingsState } from '~/reducers/editor/settings'

type Props = {
  dispatch: Function,
  settings: EditorSettingsState
}
type State = {}

class Editor extends React.PureComponent<Props, State> {
  constructo() {}
  render() {
    return (
      <Paper>
        <Grid container>
          <Grid item style={{ overflowX: 'scroll', flex: 1 }}>
            <EditorTabs />
            <EditorEditor />
          </Grid>
          {(() => {
            if (this.props.settings.opened) {
              return (
                <Grid item style={{ width: 200 }}>
                  <EditorSettings />
                </Grid>
              )
            } else {
              return (
                <Grid item style={{ width: 'auto' }}>
                  <EditorSettings />
                </Grid>
              )
            }
          })()}
        </Grid>
      </Paper>
    )
  }
}

function mapStateToProps(state) {
  return {
    settings: state.editor.settings
  }
}

export default connect(mapStateToProps)(Editor)
