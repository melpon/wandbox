// @flow
import { combineReducers } from 'redux'
import compiler from './compiler'
import compilerList from './compilerList'
import editor from './editor'
import result from './result'

export default combineReducers({
  compiler,
  compilerList,
  editor,
  result
})
