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
  selectSwitch
} from '~/actions'

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

  componentDidMount() {
    this.props.dispatch(fetchCompilerList(this.props.dispatch))
  }

  render() {
    const { compilerList, compiler } = this.props
    return (
      <SideBarComponent
        compilerList={compilerList}
        currentLanguage={compiler.currentLanguage}
        currentCompilerName={compiler.currentCompilerName}
        currentSwitches={compiler.currentSwitches}
        onChangeLanguage={this.onChangeLanguage}
        onChangeCompiler={this.onChangeCompiler}
        onChangeChecked={this.onChangeChecked}
        onChangeSelected={this.onChangeSelected}
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
