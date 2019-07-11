// @flow
import ndjsonStream from 'can-ndjson-stream'

const makeType = name => `canine/compiler/${name}`

export const SELECT_LANGUAGE = makeType('SELECT_LANGUAGE')
export function selectLanguage(language: string) {
  return {
    type: SELECT_LANGUAGE,
    language: language
  }
}

export const SELECT_COMPILER = makeType('SELECT_COMPILER')
export function selectCompiler(compilerName: string) {
  return {
    type: SELECT_COMPILER,
    compilerName: compilerName
  }
}

export const CHECK_SWITCH = makeType('CHECK_SWITCH')
export function checkSwitch(switchName: string, checked: boolean) {
  return {
    type: CHECK_SWITCH,
    switchName: switchName,
    checked: checked
  }
}

export const SELECT_SWITCH = makeType('SELECT_SWITCH')
export function selectSwitch(switchName: string, selected: string) {
  return {
    type: SELECT_SWITCH,
    switchName: switchName,
    selected: selected
  }
}

export const FETCH_COMPILER_LIST_INIT = makeType('FETCH_COMPILER_LIST_INIT')
export const FETCH_COMPILER_LIST = makeType('FETCH_COMPILER_LIST')
export async function fetchCompilerList(dispatch: Function) {
  dispatch({
    type: FETCH_COMPILER_LIST_INIT
  })

  const payload = await fetch('https://wandbox.org/api/list.json')
  return {
    type: FETCH_COMPILER_LIST,
    payload: await payload.json()
  }
}

export const COMPILE_INIT = makeType('COMPILE_INIT')
export const COMPILE_OUTPUT = makeType('COMPILE_OUTPUT')
export const COMPILE_FINISH = makeType('COMPILE_FINISH')
export async function compile(
  dispatch: Function,
  compiler: string,
  source: string,
  sources: Array<{ file: string, code: string }>,
  options: Array<string>,
  stdin: string,
  compilerOptionRaw: string,
  runtimeOptionRaw: string,
  save: boolean
) {
  dispatch({
    type: COMPILE_INIT
  })

  const body = {
    compiler: compiler,
    code: source,
    codes: sources,
    options: options.join(','),
    stdin: stdin,
    ['compiler-option-raw']: compilerOptionRaw,
    ['runtime-option-raw']: runtimeOptionRaw,
    save: save
  }

  const resp = await fetch('https://wandbox.org/api/compile.ndjson', {
    method: 'POST',
    body: JSON.stringify(body),
    mode: 'cors',
    headers: { 'content-type': 'application/json' }
  })

  const reader = ndjsonStream(resp.body).getReader()
  while (true) {
    const result = await reader.read()
    if (result.done) {
      break
    }
    dispatch({
      type: COMPILE_OUTPUT,
      payload: result.value
    })
  }

  return {
    type: COMPILE_FINISH
  }
}

export const CHANGE_COMPILER_OPTION_RAW = makeType('CHANGE_COMPILER_OPTION_RAW')
export function changeCompilerOptionRaw(compilerOptionRaw: string) {
  return {
    type: CHANGE_COMPILER_OPTION_RAW,
    compilerOptionRaw
  }
}

export const CHANGE_RUNTIME_OPTION_RAW = makeType('CHANGE_RUNTIME_OPTION_RAW')
export function changeRuntimeOptionRaw(runtimeOptionRaw: string) {
  return {
    type: CHANGE_RUNTIME_OPTION_RAW,
    runtimeOptionRaw
  }
}
export const EXPAND_RUNTIME_OPTION_RAW = makeType('EXPAND_RUNTIME_OPTION_RAW')
export function expandRuntimeOptionRaw() {
  return { type: EXPAND_RUNTIME_OPTION_RAW }
}
