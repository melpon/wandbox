// @flow
import {
  SELECT_LANGUAGE,
  SELECT_COMPILER,
  CHECK_SWITCH,
  SELECT_SWITCH,
  CHANGE_COMPILER_OPTION_RAW,
  CHANGE_RUNTIME_OPTION_RAW,
  EXPAND_RUNTIME_OPTION_RAW
} from '../actions/compiler'

export type State = {
  currentLanguage: string,
  currentCompilerName: string,
  currentSwitches: { [string]: string | boolean },
  compilerOptionRaw: string,
  runtimeOptionRaw: string,
  runtimeOptionRawExpanded: boolean
}

const initialState = {
  currentLanguage: '',
  currentCompilerName: '',
  currentSwitches: {},
  compilerOptionRaw: '',
  runtimeOptionRaw: '',
  runtimeOptionRawExpanded: false
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case SELECT_LANGUAGE:
      return {
        ...state,
        currentLanguage: action.language,
        currentCompilerName: '',
        runtimeOptionRawExpanded: false
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
    case CHANGE_COMPILER_OPTION_RAW:
      return {
        ...state,
        compilerOptionRaw: action.compilerOptionRaw
      }
    case CHANGE_RUNTIME_OPTION_RAW:
      return {
        ...state,
        runtimeOptionRaw: action.runtimeOptionRaw
      }
    case EXPAND_RUNTIME_OPTION_RAW:
      return {
        ...state,
        runtimeOptionRawExpanded: true
      }
  }
  return state
}
