// @flow
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'
import Editor from './Editor'
import Stdin from './Stdin'
import { compile } from '~/actions'
import type { State as EditorState } from '~/reducers/editor'
import type { State as CompilerState } from '~/reducers/compiler'
import type { State as ResultState } from '~/reducers/result'

type Props = {
  dispatch: Function,
  editor: EditorState,
  compiler: CompilerState,
  result: ResultState
}
type State = {}

class Main extends React.PureComponent<Props, State> {
  constructor() {
    super()
  }

  onClickRun() {
    const { editor, compiler, dispatch } = this.props
    const defaultEditor = editor.sources.find(s => s.filename == null)
    if (defaultEditor == null) {
      // 何かがおかしい
      return
    }

    dispatch(
      compile(
        dispatch,
        compiler.currentCompilerName,
        defaultEditor.text,
        editor.sources
          .filter(s => s.filename != null)
          .map(s => ({ file: s.filename || '', code: s.text })),
        [],
        editor.stdin,
        [],
        [],
        false
      )
    )
  }

  render() {
    return (
      <div>
        <Editor />
        <Stdin />
        <Button onClick={() => this.onClickRun()}>Run</Button>
        <div>
          <li>
            {this.props.result.results.map((r, index) => {
              return (
                <p key={index}>
                  {r.type} {r.data}
                </p>
              )
            })}
          </li>
        </div>
      </div>
    )
  }
}

function mapStateToProps(state) {
  return {
    compiler: state.compiler,
    editor: state.editor,
    result: state.result
  }
}

export default connect(mapStateToProps)(Main)
