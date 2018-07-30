// @flow
import {
  FETCH_COMPILER_LIST_INIT,
  FETCH_COMPILER_LIST
} from '../actions/compiler'

export type CompilerInfo = {
  name: string,
  version: string,
  language: string,
  'display-name': string,
  templates: Array<string>,
  'compiler-option-raw': boolean,
  'runtime-option-raw': boolean,
  'display-compile-command': string,
  switches: Array<
    | {|
        type: 'single',
        name: string,
        default: boolean,
        'display-flags': string,
        'display-name': string
      |}
    | {|
        type: 'select',
        name: string,
        default: string,
        options: Array<{
          name: string,
          'display-flags': string,
          'display-name': string
        }>
      |}
  >
}

export type Data = {
  compilers: Array<CompilerInfo>,
  languages: {
    [string]: Array<CompilerInfo>
  }
}

export type State = {
  loading: boolean,
  loaded: boolean,
  data: Data
}

const initialState = {
  loading: false,
  loaded: false,
  data: {
    compilers: [],
    languages: {}
  }
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case FETCH_COMPILER_LIST_INIT:
      return {
        ...state,
        loading: true
      }
    case FETCH_COMPILER_LIST:
      const compilers = (action.payload: Array<CompilerInfo>)
      const languages = {}
      for (const compiler of compilers) {
        if (languages[compiler.language] == null) {
          languages[compiler.language] = []
        }
        languages[compiler.language].push(compiler)
      }
      return {
        ...state,
        loading: false,
        loaded: true,
        data: {
          compilers: compilers,
          languages: languages
        }
      }
  }
  return state
}
