// @flow
import React from 'react'
import { withStyles } from '@material-ui/core/styles'
import Select from '@material-ui/core/Select'
import MenuItem from '@material-ui/core/MenuItem'
import FormControlLabel from '@material-ui/core/FormControlLabel'
import Checkbox from '@material-ui/core/Checkbox'
import type { State as CompilerListState } from '../reducers/compilerList'

const styles = _theme => ({})

type Props = {
  classes: Object,
  currentLanguage: string,
  currentCompilerName: string,
  currentSwitches: { [string]: boolean | string },
  compilerList: CompilerListState,
  onChangeLanguage: string => void,
  onChangeCompiler: string => void,
  onChangeChecked: (string, string) => void,
  onChangeSelected: (string, string) => void
}

const SideBar = (props: Props) => {
  const {
    classes: _classes,
    compilerList,
    onChangeLanguage,
    onChangeCompiler,
    onChangeChecked,
    onChangeSelected,
    currentLanguage,
    currentCompilerName,
    currentSwitches
  } = props

  return (
    <div>
      <Select
        value={currentLanguage}
        onChange={e => onChangeLanguage(e.target.value)}
      >
        {Object.keys(compilerList.data.languages)
          .sort()
          .map(lang => {
            return (
              <MenuItem key={lang} value={lang}>
                {lang}
              </MenuItem>
            )
          })}
      </Select>
      {(() => {
        if (currentLanguage == '') {
          return null
        }

        const infos = compilerList.data.languages[currentLanguage]
        if (infos == null) {
          return null
        }

        return (
          <Select
            value={currentCompilerName}
            onChange={e => onChangeCompiler(e.target.value)}
          >
            {infos.map(info => {
              return (
                <MenuItem key={info.name} value={info.name}>
                  {`${info['display-name']} ${info.version}`}
                </MenuItem>
              )
            })}
          </Select>
        )
      })()}
      {(() => {
        if (currentCompilerName == '') {
          return null
        }

        const info = compilerList.data.compilers.find(
          compiler => compiler.name == currentCompilerName
        )
        if (info == null) {
          return null
        }

        return (
          <div>
            {info.switches.map(sw => {
              if (sw.type === 'single') {
                // checkbox
                const checked =
                  sw.name in currentSwitches
                    ? currentSwitches[sw.name]
                    : sw.default
                return (
                  <FormControlLabel
                    key={sw.name}
                    control={
                      <Checkbox
                        checked={checked}
                        onChange={e =>
                          onChangeChecked(sw.name, e.target.checked)
                        }
                        value={sw.name}
                      />
                    }
                    label={sw['display-name']}
                  />
                )
              } else if (sw.type === 'select') {
                // select
                const value = (() => {
                  if (!(sw.name in currentSwitches)) {
                    return sw.default
                  }
                  const name = currentSwitches[sw.name]
                  if (typeof name !== 'string') {
                    return sw.default
                  }
                  if (sw.options.find(opt => opt.name == name) === undefined) {
                    return sw.default
                  }
                  return name
                })()
                return (
                  <Select
                    key={sw.name}
                    value={value}
                    onChange={e => onChangeSelected(sw.name, e.target.value)}
                  >
                    {sw.options.map(opt => {
                      return (
                        <MenuItem key={opt.name} value={opt.name}>
                          {opt['display-name']}
                        </MenuItem>
                      )
                    })}
                  </Select>
                )
              }
            })}
          </div>
        )
      })()}
    </div>
  )
}

export default withStyles(styles)(SideBar)
