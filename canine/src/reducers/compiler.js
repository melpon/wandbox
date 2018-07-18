// @flow
import {
  SELECT_LANGUAGE,
  SELECT_COMPILER,
  CHECK_SWITCH,
  SELECT_SWITCH
} from '../actions'

export type State = {
  currentLanguage: string,
  currentCompilerName: string,
  currentSwitches: { [string]: string | boolean }
}

const initialState = {
  currentLanguage: '',
  currentCompilerName: '',
  currentSwitches: {}
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case SELECT_LANGUAGE:
      return {
        ...state,
        currentLanguage: action.language
      }
    case SELECT_COMPILER:
      return {
        ...state,
        currentCompilerName: action.compilerName
      }
    case CHECK_SWITCH: {
      return {
        ...state,
        currentSwitches: {
          ...state.currentSwitches,
          [action.switchName]: action.checked
        }
      }
    }
    case SELECT_SWITCH: {
      return {
        ...state,
        currentSwitches: {
          ...state.currentSwitches,
          [action.switchName]: action.selected
        }
      }
    }
  }
  return state
}
