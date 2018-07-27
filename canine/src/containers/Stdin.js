// @flow
import React from 'react'
import { connect } from 'react-redux'
import StdinComponent from '~/components/Stdin'
import { changeStdin } from '~/actions/editor'
import type { State as EditorState } from '~/reducers/editor'

type Props = {
  dispatch: Function,
  editor: EditorState
}
type State = {
  open: boolean
}

class Stdin extends React.PureComponent<Props, State> {
  constructor() {
    super()
    this.onChange = this.onChange.bind(this)
    this.onOpen = this.onOpen.bind(this)
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

  render() {
    return (
      <StdinComponent
        open={this.state.open}
        stdin={this.props.editor.stdin}
        onOpen={this.onOpen}
        onChange={this.onChange}
      />
    )
  }
}

function mapStateToProps(state) {
  return {
    editor: state.editor
  }
}

export default connect(mapStateToProps)(Stdin)
