// @flow
import {
  COMPILE_INIT,
  COMPILE_OUTPUT,
  COMPILE_FINISH
} from '../actions/compiler'

export type State = {
  compiling: boolean,
  results: Array<{ type: string, data: string }>
}

const initialState = {
  compiling: false,
  results: []
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case COMPILE_INIT:
      return {
        ...state,
        compiling: true,
        results: []
      }
    case COMPILE_OUTPUT:
      return {
        ...state,
        results: [
          ...state.results,
          { type: action.payload.type, data: action.payload.data }
        ]
      }
    case COMPILE_FINISH:
      return {
        ...state,
        compiling: false
      }
  }
  return state
}
