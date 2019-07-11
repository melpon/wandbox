// @flow
import { createStore, applyMiddleware } from 'redux'
import promiseMiddleware from 'redux-promise'
import { createLogger } from 'redux-logger'
import reducer from './reducers'

const loggerMiddleware = createLogger()

export default function configureStore() {
  return createStore(
    reducer,
    applyMiddleware(promiseMiddleware, loggerMiddleware)
  )
}
