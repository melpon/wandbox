// @flow
import React from 'react'
import { withStyles } from '@material-ui/core/styles'
import { Router as ReachRouter } from '@reach/router'
import TopBar from './TopBar'
import Home from './Home'

const styles = _theme => {}

type Props = {}

const Router = (_props: Props) => {
  return (
    <div>
      <TopBar />
      <ReachRouter>
        <Home path="/" />
      </ReachRouter>
    </div>
  )
}

export default withStyles(styles)(Router)
