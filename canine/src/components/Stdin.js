// @flow
import React from 'react'
import Grid from '@material-ui/core/Grid'
import { withStyles } from '@material-ui/core/styles'
import CodeMirror from './CodeMirror'
import Button from '@material-ui/core/Button'

const styles = _theme => ({})

type Props = {
  open: boolean,
  stdin: string,
  onOpen: () => void,
  onChange: string => void
}

const Stdin = (props: Props) => {
  const { open, stdin, onOpen, onChange } = props
  return (
    <Grid container>
      {(() => {
        if (!open && stdin.length == 0) {
          return <Button onClick={() => onOpen()}>Stdin</Button>
        } else {
          return (
            <CodeMirror
              value={stdin}
              options={{
                viewportMargin: Infinity,
                smartIndent: false
              }}
              onBeforeChange={(_cm, _data, value) => onChange(value)}
            />
          )
        }
      })()}
    </Grid>
  )
}

export default withStyles(styles)(Stdin)
