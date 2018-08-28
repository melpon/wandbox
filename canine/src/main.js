// @flow
import 'core-js/shim'
import 'regenerator-runtime/runtime'
import React from 'react'
import ReactDOM from 'react-dom'
import moment from 'moment-timezone'
import App from './containers/App'
import configureStore from './configureStore'

const store = configureStore()
const root = document.querySelector('#root')

moment.locale('en')

if (root != null) {
  ReactDOM.render(<App store={store} />, root)
}
