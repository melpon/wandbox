// @flow
import React from 'react'
import { connect } from 'react-redux'
import StdinComponent from '~/components/Stdin'
import { changeStdin } from '~/actions/editor'
import { compile } from '~/utils'
import type { State as EditorState } from '~/reducers/editor'
import type { State as CompilerState } from '~/reducers/compiler'
import type { State as CompilerListState } from '~/reducers/compilerList'

type Props = {
  dispatch: Function,
  editor: EditorState,
  compiler: CompilerState,
  compilerList: CompilerListState
}
type State = {
  open: boolean
}

class Stdin extends React.PureComponent<Props, State> {
  constructor() {
    super()
    this.onChange = this.onChange.bind(this)
    this.onOpen = this.onOpen.bind(this)
    this.onCtrlEnter = this.onCtrlEnter.bind(this)
    this.state = {
      open: false
    }
  }

  onChange: string => void
  onChange(stdin) {
    this.props.dispatch(changeStdin(stdin))
  }

  onOpen: () => void
  onOpen() {
    this.setState({
      open: true
    })
  }

  onCtrlEnter: () => void
  onCtrlEnter() {
    const { dispatch, editor, compiler, compilerList } = this.props
    compile(dispatch, editor, compiler, compilerList)
  }

  render() {
    return (
      <StdinComponent
        open={this.state.open}
        stdin={this.props.editor.stdin}
        onOpen={this.onOpen}
        onChange={this.onChange}
        onCtrlEnter={this.onCtrlEnter}
      />
    )
  }
}

function mapStateToProps(state) {
  return {
    editor: state.editor,
    compiler: state.compiler,
    compilerList: state.compilerList
  }
}

export default connect(mapStateToProps)(Stdin)
