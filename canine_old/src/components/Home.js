// @flow
import React from 'react'
import Grid from '@material-ui/core/Grid'
import { withStyles } from '@material-ui/core/styles'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/material.css'

const styles = theme => ({
  content: {
    backgroundColor: theme.palette.background.default
  },
  toolbar: theme.mixins.toolbar
})

type Props = {
  SideBar: typeof React.Component,
  Main: typeof React.Component,
  classes: Object
}

const Home = (props: Props) => {
  const { SideBar, Main, classes } = props
  return (
    <Grid container>
      <Grid item xs={2}>
        <div className={classes.toolbar} />
        <SideBar />
      </Grid>
      <Grid item xs={10}>
        <main className={classes.content}>
          <div className={classes.toolbar} />
          <Main />
        </main>
      </Grid>
    </Grid>
  )
}

export default withStyles(styles)(Home)
