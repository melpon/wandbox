// @flow
import React from 'react'
import { connect } from 'react-redux'
import TopBarComponent from '../components/TopBar'

type Props = {}
type State = {}

class TopBar extends React.Component<Props, State> {
  render() {
    return <TopBarComponent />
  }
}

function mapStateToProps(_state) {
  return {}
}

export default connect(mapStateToProps)(TopBar)
