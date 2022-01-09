// @flow
import React from 'react'
import { connect } from 'react-redux'
import { reduceCompileOptions } from '~/utils'
import type { State as CompilerState } from '~/reducers/compiler'
import type { State as CompilerListState } from '~/reducers/compilerList'

type Props = {
  compiler: CompilerState,
  compilerList: CompilerListState
}
type State = {}

class Command extends React.PureComponent<Props, State> {
  rawToOptions(raw: string): string {
    return raw.split('\n').join(' ')
  }

  getCommand(): string {
    const { compiler, compilerList } = this.props
    const info = compilerList.data.compilers.find(
      c => c.name == compiler.currentCompilerName
    )
    if (info === undefined) {
      return ''
    }

    const command = info['display-compile-command']
    const options = reduceCompileOptions(
      compiler.currentSwitches,
      info,
      [],
      (sw, state) => [...state, sw['display-flags']],
      (sw, value, state) => {
        const opt = sw.options.find(opt => opt.name == value)
        if (opt === undefined) {
          throw 'something wrong'
        }
        return [...state, opt['display-flags']]
      }
    )
    const rawOptions = this.rawToOptions(
      info['compiler-option-raw']
        ? compiler.compilerOptionRaw
        : compiler.runtimeOptionRaw
    )
    return `$ ${command} ${options.join(' ')} ${rawOptions}`
  }

  render() {
    const command = this.getCommand()
    return <code>{command}</code>
  }
}

function mapStateToProps(state) {
  return {
    compiler: state.compiler,
    compilerList: state.compilerList
  }
}

export default connect(mapStateToProps)(Command)
