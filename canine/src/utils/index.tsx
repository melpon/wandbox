/*
export function compile(
  dispatch: Function,
  editor: EditorState,
  compiler: CompilerState,
  compilerList: CompilerListState
) {
  const defaultEditor = editor.sources.find(s => s.filename == null)
  if (defaultEditor == null) {
    // something wrong
    return
  }

  if (!compilerList.loaded) {
    return
  }

  const info = compilerList.data.compilers.find(
    c => c.name == compiler.currentCompilerName
  )
  if (info === undefined) {
    return
  }

  // get options
  const options = getCompileOptions(compiler.currentSwitches, info)

  dispatch(
    compileAction(
      dispatch,
      compiler.currentCompilerName,
      defaultEditor.text,
      editor.sources
        .filter(s => s.filename != null)
        .map(s => ({ file: s.filename || '', code: s.text })),
      options,
      editor.stdin,
      compiler.compilerOptionRaw,
      compiler.runtimeOptionRaw,
      false
    )
  )
}
*/
