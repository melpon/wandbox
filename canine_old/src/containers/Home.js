// @flow
import React from 'react'
import HomeComponent from '~/components/Home'
import { connect } from 'react-redux'
import SideBar from './SideBar'
import Main from './Main'
import type { State as EditorState } from '~/reducers/editor'

type Props = {
  dispatch: Function,
  editor: EditorState
}
type State = {}

class Home extends React.PureComponent<Props, State> {
  constructor() {
    super()
  }

  render() {
    //const { classes } = props
    return <HomeComponent SideBar={SideBar} Main={Main} />
  }
}

function mapStateToProps(state) {
  return {
    editor: state.editor
  }
}

export default connect(mapStateToProps)(Home)
