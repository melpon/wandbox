// @flow
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'
import Editor from './Editor'
import Stdin from './Stdin'
import Command from './Command'
import { compile } from '~/utils'
import type { State as EditorState } from '~/reducers/editor'
import type { State as CompilerState } from '~/reducers/compiler'
import type { State as CompilerListState } from '~/reducers/compilerList'
import type { State as ResultState } from '~/reducers/result'

type Props = {
  dispatch: Function,
  editor: EditorState,
  compiler: CompilerState,
  compilerList: CompilerListState,
  result: ResultState
}
type State = {}

class Main extends React.PureComponent<Props, State> {
  onClickRun() {
    const { dispatch, editor, compiler, compilerList } = this.props
    compile(dispatch, editor, compiler, compilerList)
  }

  render() {
    return (
      <div>
        <Editor />
        <Stdin />
        <Command />
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
    compilerList: state.compilerList,
    editor: state.editor,
    result: state.result
  }
}

export default connect(mapStateToProps)(Main)
