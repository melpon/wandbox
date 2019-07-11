// @flow
import React from 'react'
import { connect } from 'react-redux'
import EditorTabsComponent from '~/components/EditorTabs'
import {
  changeEditorTab,
  addEditor,
  closeTab,
  beginRename,
  changeRename,
  cancelRename,
  submitRename
} from '~/actions/editor/tabs'
import type {
  State as EditorState,
  Source as EditorSource
} from '~/reducers/editor'

type Props = {
  dispatch: Function,
  editor: EditorState
}
type State = {}

class Tabs extends React.PureComponent<Props, State> {
  constructor() {
    super()
    this.onChangeTabs = this.onChangeTabs.bind(this)
    this.onClickTabEdit = this.onClickTabEdit.bind(this)
    this.onClickTabClose = this.onClickTabClose.bind(this)
    this.onChangeRenamingFilename = this.onChangeRenamingFilename.bind(this)
    this.onCancelRenamingFilename = this.onCancelRenamingFilename.bind(this)
    this.onSubmitRenamingFilename = this.onSubmitRenamingFilename.bind(this)
  }

  onChangeTabs: number => void
  onChangeTabs(index) {
    // 追加ボタン
    if (index == this.props.editor.sources.length) {
      this.props.dispatch(addEditor())
    } else {
      this.props.dispatch(changeEditorTab(index))
    }
  }

  onClickTabClose: EditorSource => void
  onClickTabClose(source) {
    this.props.dispatch(closeTab(source))
  }

  onClickTabEdit: EditorSource => void
  onClickTabEdit(source) {
    this.props.dispatch(beginRename(source))
  }
  onChangeRenamingFilename: (EditorSource, string) => void
  onChangeRenamingFilename(source, filename) {
    this.props.dispatch(changeRename(source, filename))
  }
  onCancelRenamingFilename: EditorSource => void
  onCancelRenamingFilename(source) {
    this.props.dispatch(cancelRename(source))
  }
  onSubmitRenamingFilename: EditorSource => void
  onSubmitRenamingFilename(source) {
    this.props.dispatch(submitRename(source))
  }

  render() {
    return (
      <EditorTabsComponent
        currentTab={this.props.editor.currentTab}
        sources={this.props.editor.sources}
        onChangeTabs={this.onChangeTabs}
        onClickTabEdit={this.onClickTabEdit}
        onClickTabClose={this.onClickTabClose}
        onChangeRenamingFilename={this.onChangeRenamingFilename}
        onCancelRenamingFilename={this.onCancelRenamingFilename}
        onSubmitRenamingFilename={this.onSubmitRenamingFilename}
      />
    )
  }
}

function mapStateToProps(state) {
  return {
    editor: state.editor,
    compiler: state.compiler
  }
}

export default connect(mapStateToProps)(Tabs)
