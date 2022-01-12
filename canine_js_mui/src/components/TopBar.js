// @flow
import React from 'react'
import { withStyles } from '@material-ui/core/styles'
import AppBar from '@material-ui/core/AppBar'
import Toolbar from '@material-ui/core/Toolbar'
import Typography from '@material-ui/core/Typography'
import { Link } from '@reach/router'

const drawerWidth = 240

const styles = theme => ({
  appBar: {
    zIndex: theme.zIndex.drawer + 1
  },
  brand: {
    textDecoration: 'none'
  },
  drawerPaper: {
    position: 'relative',
    width: drawerWidth
  },
  toolbar: theme.mixins.toolbar
})

type Props = {
  classes: Object
}

const TopBar = (props: Props) => {
  const { classes } = props

  return (
    <AppBar position="absolute" className={classes.appBar}>
      <Toolbar>
        {/*
          <IconButton color="inherit" aria-label="Menu">
            <MenuIcon />
          </IconButton>
          */}
        <Typography
          className={classes.brand}
          variant="title"
          color="inherit"
          component={Link}
          to="/"
        >
          Wandbox
        </Typography>
      </Toolbar>
    </AppBar>
  )
}

export default withStyles(styles)(TopBar)
