// @flow
import React from 'react'
import { connect } from 'react-redux'
import Button from '@material-ui/core/Button'
import Editor from './Editor'
import Stdin from './Stdin'
import { compile } from '~/actions/compiler'
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
  constructor() {
    super()
  }

  onClickRun() {
    const { editor, compiler, compilerList, dispatch } = this.props
    const defaultEditor = editor.sources.find(s => s.filename == null)
    if (defaultEditor == null) {
      // something wrong
      return
    }

    if (!compilerList.loaded) {
      return
    }

    const info = compilerList.data.compilers.find(
      c => c.name == compiler.currentCompilerName
    )
    if (info === undefined) {
      return
    }

    // get options
    const options = (() => {
      const opts = []
      for (const sw of info.switches) {
        if (sw.type === 'single') {
          if (sw.name in compiler.currentSwitches) {
            if (compiler.currentSwitches[sw.name]) {
              opts.push(sw.name)
            }
          } else {
            if (sw.default) {
              opts.push(sw.name)
            }
          }
        } else {
          if (sw.name in compiler.currentSwitches) {
            const value = compiler.currentSwitches[sw.name]
            if (typeof value === 'string') {
              opts.push(value)
            }
          } else {
            opts.push(sw.default)
          }
        }
      }
      return opts
    })()

    dispatch(
      compile(
        dispatch,
        compiler.currentCompilerName,
        defaultEditor.text,
        editor.sources
          .filter(s => s.filename != null)
          .map(s => ({ file: s.filename || '', code: s.text })),
        options,
        editor.stdin,
        compiler.compilerOptionRaw,
        compiler.runtimeOptionRaw,
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
    compilerList: state.compilerList,
    editor: state.editor,
    result: state.result
  }
}

export default connect(mapStateToProps)(Main)
