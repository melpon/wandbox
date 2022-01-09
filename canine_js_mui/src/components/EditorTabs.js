// @flow
import React from 'react'
import { withStyles } from '@material-ui/core/styles'
import Typography from '@material-ui/core/Typography'
import IconButton from '@material-ui/core/IconButton'
import Tabs from '@material-ui/core/Tabs'
import Tab from '@material-ui/core/Tab'
import TextField from '@material-ui/core/TextField'
import InsertDriveFileIcon from '@material-ui/icons/InsertDriveFile'
import AddBoxIcon from '@material-ui/icons/AddBox'
import EditIcon from '@material-ui/icons/Edit'
import ClearIcon from '@material-ui/icons/Clear'
import CheckIcon from '@material-ui/icons/Check'
import type { Source as EditorSource } from '~/reducers/editor'
import 'codemirror/lib/codemirror.css'
import 'codemirror/theme/material.css'

const styles = _theme => ({
  tabRoot: {
    textTransform: 'initial'
  }
})

type Props = {
  classes: Object,
  currentTab: number,
  sources: Array<EditorSource>,
  onChangeTabs: number => void,
  onClickTabEdit: EditorSource => void,
  onClickTabClose: EditorSource => void,
  onChangeRenamingFilename: (EditorSource, string) => void,
  onCancelRenamingFilename: EditorSource => void,
  onSubmitRenamingFilename: EditorSource => void
}

const TabComponent = (props: any) => {
  const {
    source: s,
    onClickTabEdit,
    onClickTabClose,
    onChangeRenamingFilename,
    onCancelRenamingFilename,
    onSubmitRenamingFilename,
    ...otherProps
  } = props
  return (
    <span {...otherProps}>
      <InsertDriveFileIcon />
      {(() => {
        if (!s.renaming) {
          return (
            <span>
              <Typography variant="body1">{s.filename || ''}</Typography>
              {(() => {
                if (s.filename != null) {
                  return (
                    <span>
                      <IconButton
                        onClick={e => {
                          onClickTabEdit(s)
                          e.stopPropagation()
                        }}
                      >
                        <EditIcon />
                      </IconButton>
                      <IconButton
                        onClick={e => {
                          onClickTabClose(s)
                          e.stopPropagation()
                        }}
                      >
                        <ClearIcon />
                      </IconButton>
                    </span>
                  )
                }
              })()}
            </span>
          )
        } else {
          return (
            <span>
              <TextField
                label="Filename"
                value={s.renamingFilename}
                onClick={e => e.stopPropagation()}
                onChange={e => onChangeRenamingFilename(s, e.target.value)}
              />
              <ClearIcon onClick={() => onCancelRenamingFilename(s)} />
              <CheckIcon onClick={() => onSubmitRenamingFilename(s)} />
            </span>
          )
        }
      })()}
    </span>
  )
}

const EditorTabs = (props: Props) => {
  const {
    classes,
    currentTab,
    sources,
    onChangeTabs,
    onClickTabEdit,
    onClickTabClose,
    onChangeRenamingFilename,
    onCancelRenamingFilename,
    onSubmitRenamingFilename
  } = props

  return (
    <Tabs
      value={currentTab}
      onChange={(_, index) => onChangeTabs(index)}
      indicatorColor="primary"
      textColor="primary"
      scrollable
      scrollButtons="auto"
    >
      {sources.map((s, index) => {
        return (
          <Tab
            key={index}
            classes={{ root: classes.tabRoot }}
            source={s}
            component={TabComponent}
            onClickTabEdit={onClickTabEdit}
            onClickTabClose={onClickTabClose}
            onChangeRenamingFilename={onChangeRenamingFilename}
            onCancelRenamingFilename={onCancelRenamingFilename}
            onSubmitRenamingFilename={onSubmitRenamingFilename}
          />
        )
      })}
      <Tab key={sources.length} icon={<AddBoxIcon />} />
    </Tabs>
  )
}

export default withStyles(styles)(EditorTabs)
