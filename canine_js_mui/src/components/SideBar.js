// @flow
import React from 'react'
import { withStyles } from '@material-ui/core/styles'
import Select from '@material-ui/core/Select'
import MenuItem from '@material-ui/core/MenuItem'
import FormControlLabel from '@material-ui/core/FormControlLabel'
import Checkbox from '@material-ui/core/Checkbox'
import Button from '@material-ui/core/Button'
import CodeMirror from './CodeMirror'
import type { State as CompilerListState } from '../reducers/compilerList'
import type { State as CompilerState } from '../reducers/compiler'

const styles = _theme => ({})

type Props = {
  classes: Object,
  compilerList: CompilerListState,
  compiler: CompilerState,
  onChangeLanguage: string => void,
  onChangeCompiler: string => void,
  onChangeChecked: (string, string) => void,
  onChangeSelected: (string, string) => void,
  onChangeCompilerOptionRaw: string => void,
  onChangeRuntimeOptionRaw: string => void,
  onExpandRuntimeOptionRaw: () => void,
  onCtrlEnter: () => void
}

const SideBar = (props: Props) => {
  const {
    classes: _classes,
    compilerList,
    compiler,
    onChangeLanguage,
    onChangeCompiler,
    onChangeChecked,
    onChangeSelected,
    onChangeCompilerOptionRaw,
    onChangeRuntimeOptionRaw,
    onExpandRuntimeOptionRaw,
    onCtrlEnter
  } = props
  const {
    currentLanguage,
    currentCompilerName,
    currentSwitches,
    compilerOptionRaw,
    runtimeOptionRaw,
    runtimeOptionRawExpanded
  } = compiler

  return (
    <div>
      {/* choose language */}
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

      {/* choose compiler */}
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

      {/* compiler options */}
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

      {/* compiler/runtime options raw */}
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

        let compilerComponent = null
        if (info['compiler-option-raw']) {
          compilerComponent = (
            <CodeMirror
              value={compilerOptionRaw}
              options={{
                viewportMargin: Infinity,
                smartIndent: false,
                extraKeys: {
                  'Ctrl-Enter': () => {
                    onCtrlEnter()
                  }
                }
              }}
              onBeforeChange={(_cm, _data, value) =>
                onChangeCompilerOptionRaw(value)
              }
              expand={false}
            />
          )
        }

        let runtimeComponent = null
        if (info['runtime-option-raw'] || runtimeOptionRawExpanded) {
          runtimeComponent = (
            <CodeMirror
              value={runtimeOptionRaw}
              options={{
                viewportMargin: Infinity,
                smartIndent: false,
                extraKeys: {
                  'Ctrl-Enter': () => {
                    onCtrlEnter()
                  }
                }
              }}
              onBeforeChange={(_cm, _data, value) =>
                onChangeRuntimeOptionRaw(value)
              }
              expand={false}
            />
          )
        } else {
          runtimeComponent = (
            <Button onClick={() => onExpandRuntimeOptionRaw()}>
              Runtime options...
            </Button>
          )
        }
        return (
          <div>
            {compilerComponent}
            {runtimeComponent}
          </div>
        )
      })()}
    </div>
  )
}

export default withStyles(styles)(SideBar)
