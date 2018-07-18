// @flow
import React from 'react'
import { connect } from 'react-redux'
import EditorComponent from '~/components/Editor'
import {
  changeEditorText,
  changeEditorTab,
  addEditor,
  closeTab,
  beginRename,
  changeRename,
  cancelRename,
  submitRename
} from '~/actions/editor'
import type { State as EditorState } from '~/reducers/editor'
import type { Source as EditorSource } from '~/reducers/editor'

type Props = {
  dispatch: Function,
  editor: EditorState
}
type State = {}

class Editor extends React.PureComponent<Props, State> {
  constructor() {
    super()
    this.onChangeEditorText = this.onChangeEditorText.bind(this)
    this.onChangeTabs = this.onChangeTabs.bind(this)
    this.onClickTabEdit = this.onClickTabEdit.bind(this)
    this.onClickTabClose = this.onClickTabClose.bind(this)
    this.onChangeRenamingFilename = this.onChangeRenamingFilename.bind(this)
    this.onCancelRenamingFilename = this.onCancelRenamingFilename.bind(this)
    this.onSubmitRenamingFilename = this.onSubmitRenamingFilename.bind(this)
  }

  onChangeEditorText: (string | null, string) => void
  onChangeEditorText(filename, text) {
    this.props.dispatch(changeEditorText(filename, text))
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
    //const { classes } = props
    return (
      <EditorComponent
        currentTab={this.props.editor.currentTab}
        sources={this.props.editor.sources}
        onChangeTabs={this.onChangeTabs}
        onChangeEditorText={this.onChangeEditorText}
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
    editor: state.editor
  }
}

export default connect(mapStateToProps)(Editor)
