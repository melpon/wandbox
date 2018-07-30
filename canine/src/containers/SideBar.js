// @flow
import React from 'react'
import { connect } from 'react-redux'
import SideBarComponent from '~/components/SideBar'
import type { State as CompilerListState } from '~/reducers/compilerList'
import type { State as CompilerState } from '~/reducers/compiler'
import {
  fetchCompilerList,
  selectLanguage,
  selectCompiler,
  checkSwitch,
  selectSwitch,
  changeCompilerOptionRaw,
  changeRuntimeOptionRaw,
  expandRuntimeOptionRaw
} from '~/actions/compiler'

type Props = {
  dispatch: Function,
  compilerList: CompilerListState,
  compiler: CompilerState
}
type State = {}

class SideBar extends React.Component<Props, State> {
  constructor() {
    super()
    this.onChangeLanguage = this.onChangeLanguage.bind(this)
    this.onChangeCompiler = this.onChangeCompiler.bind(this)
    this.onChangeChecked = this.onChangeChecked.bind(this)
    this.onChangeSelected = this.onChangeSelected.bind(this)
    this.onChangeCompilerOptionRaw = this.onChangeCompilerOptionRaw.bind(this)
    this.onChangeRuntimeOptionRaw = this.onChangeRuntimeOptionRaw.bind(this)
    this.onExpandRuntimeOptionRaw = this.onExpandRuntimeOptionRaw.bind(this)
  }

  onChangeLanguage: string => void
  onChangeLanguage(language: string) {
    this.props.dispatch(selectLanguage(language))
  }

  onChangeCompiler: string => void
  onChangeCompiler(compilerName: string) {
    this.props.dispatch(selectCompiler(compilerName))
  }

  onChangeChecked: (string, boolean) => void
  onChangeChecked(switchName, checked) {
    this.props.dispatch(checkSwitch(switchName, checked))
  }

  onChangeSelected: (string, string) => void
  onChangeSelected(switchName, selected) {
    this.props.dispatch(selectSwitch(switchName, selected))
  }

  onChangeCompilerOptionRaw: string => void
  onChangeCompilerOptionRaw(compilerOptionRaw: string) {
    this.props.dispatch(changeCompilerOptionRaw(compilerOptionRaw))
  }

  onChangeRuntimeOptionRaw: string => void
  onChangeRuntimeOptionRaw(runtimeOptionRaw: string) {
    this.props.dispatch(changeRuntimeOptionRaw(runtimeOptionRaw))
  }

  onExpandRuntimeOptionRaw: () => void
  onExpandRuntimeOptionRaw() {
    this.props.dispatch(expandRuntimeOptionRaw())
  }

  componentDidMount() {
    this.props.dispatch(fetchCompilerList(this.props.dispatch))
  }

  render() {
    const { compilerList, compiler } = this.props
    return (
      <SideBarComponent
        compilerList={compilerList}
        compiler={compiler}
        onChangeLanguage={this.onChangeLanguage}
        onChangeCompiler={this.onChangeCompiler}
        onChangeChecked={this.onChangeChecked}
        onChangeSelected={this.onChangeSelected}
        onChangeCompilerOptionRaw={this.onChangeCompilerOptionRaw}
        onChangeRuntimeOptionRaw={this.onChangeRuntimeOptionRaw}
        onExpandRuntimeOptionRaw={this.onExpandRuntimeOptionRaw}
      />
    )
  }
}

function mapStateToProps(state) {
  return {
    compilerList: state.compilerList,
    compiler: state.compiler
  }
}

export default connect(mapStateToProps)(SideBar)
