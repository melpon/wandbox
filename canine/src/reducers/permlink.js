// @flow
import {
  fetchPermlink,
  FETCH_PERMLINK_INIT,
  FETCH_PERMLINK
} from '~/actions/permlink'

export type Source = {
  filename: string | null,
  text: string,
  renaming: boolean,
  renamingFilename: string
}

export type Permlink = {}

export type State = {
  usingPermlink: false,
  sources: Array<Source>,
  stdin: string,
  settings: SettingsState
}
const initialState = {
  currentTab: 0,
  sources: [
    {
      filename: null,
      text: '',
      renaming: false,
      renamingFilename: ''
    }
  ],
  stdin: '',
  settings: settingsInitialState
}
export default function(state: State = initialState, action: Object): State {
  switch (action.type) {
    case CHANGE_EDITOR_TAB:
      return { ...state, currentTab: action.tab }
    case CHANGE_EDITOR_TEXT: {
      // copy and mutate
      const sources = [...state.sources]
      const index = sources.findIndex(s => s.filename == action.filename)
      if (index !== undefined) {
        sources[index] = {
          ...sources[index],
          text: action.text
        }
        return {
          ...state,
          sources: sources
        }
      } else {
        return state
      }
    }
    case ADD_EDITOR: {
      const filename = (() => {
        // find uniqe name
        let n = 1
        while (true) {
          const filename = `noname-${n}`
          const found = state.sources.find(s => s.filename == filename)
          if (found === undefined) {
            return filename
          }
          n += 1
        }
        // unreachable
        throw ''
      })()
      const source = {
        filename: filename,
        text: '',
        renaming: false,
        renamingFilename: ''
      }
      return { ...state, sources: [...state.sources, source] }
    }
    case CLOSE_TAB: {
      // remove selected source
      const tab = state.sources.findIndex(
        s => s.filename == action.source.filename
      )
      if (tab === undefined) {
        return state
      }

      const sources = [...state.sources]
      sources.splice(tab, 1)

      // adjust currentTab if it is out-of-index
      const currentTab =
        state.currentTab == sources.length
          ? state.currentTab - 1
          : state.currentTab
      console.log(currentTab)
      return {
        ...state,
        currentTab,
        sources
      }
    }
    case BEGIN_RENAME: {
      const tab = state.sources.findIndex(
        s => s.filename == action.source.filename
      )
      if (tab === undefined) {
        return state
      }

      const sources = [...state.sources]
      const source = sources[tab]
      if (source.filename === null) {
        return state
      }

      sources[tab] = {
        ...source,
        renaming: true,
        renamingFilename: source.filename
      }
      return {
        ...state,
        currentTab: tab,
        sources: sources
      }
    }
    case CHANGE_RENAME: {
      const tab = state.sources.findIndex(
        s => s.filename == action.source.filename
      )
      if (tab === undefined) {
        return state
      }

      const sources = [...state.sources]
      const source = sources[tab]

      sources[tab] = {
        ...source,
        renamingFilename: action.filename
      }
      return {
        ...state,
        sources: sources
      }
    }
    case CANCEL_RENAME: {
      const tab = state.sources.findIndex(
        s => s.filename == action.source.filename
      )
      if (tab === undefined) {
        return state
      }

      const sources = [...state.sources]
      const source = sources[tab]

      sources[tab] = {
        ...source,
        renaming: false,
        renamingFilename: ''
      }
      return {
        ...state,
        sources: sources
      }
    }
    case SUBMIT_RENAME: {
      const tab = state.sources.findIndex(
        s => s.filename == action.source.filename
      )
      if (tab === undefined) {
        return state
      }

      const sources = [...state.sources]
      const source = sources[tab]

      let filename = normalizePath(source.renamingFilename)
      if (filename.length == 0) {
        filename = source.filename
      }

      sources[tab] = {
        ...source,
        filename: filename,
        renaming: false,
        renamingFilename: ''
      }
      return {
        ...state,
        sources: sources
      }
    }
    case CHANGE_STDIN:
      return {
        ...state,
        stdin: action.stdin
      }
    default:
      return {
        ...state,
        settings: settings(state.settings, action)
      }
  }
}
