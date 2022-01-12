// @flow
import React from 'react'
import { Provider, connect } from 'react-redux'
import { MuiThemeProvider, createMuiTheme } from '@material-ui/core/styles'
import purple from '@material-ui/core/colors/purple'
import green from '@material-ui/core/colors/green'
import Router from './Router'

const theme = createMuiTheme({
  palette: {
    primary: purple,
    secondary: green
  },
  status: {
    danger: 'orange'
  }
})

type Props = {
  store: Object
}

const App = (props: Props) => {
  return (
    <Provider store={props.store}>
      <MuiThemeProvider theme={theme}>
        <Router />
      </MuiThemeProvider>
    </Provider>
  )
}

function mapStateToProps(_state) {
  return {}
}

export default connect(mapStateToProps)(App)
