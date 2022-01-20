import { CompilerInfo, SingleSwitch, SelectSwitch } from "~/hooks/compilerList";

export function reduceCompileOptions<T>(
  currentSwitches: { [name: string]: string | boolean },
  info: CompilerInfo,
  state: T,
  singleFunc: (sw: SingleSwitch, state: T) => T,
  selectFunc: (sw: SelectSwitch, name: string, state: T) => T
): T {
  for (const sw of info.switches) {
    if (sw.type === "single") {
      const ssw = sw.switch as SingleSwitch;
      if (ssw.name in currentSwitches) {
        if (currentSwitches[ssw.name]) {
          state = singleFunc(ssw, state);
        }
      } else {
        if (ssw.default) {
          state = singleFunc(ssw, state);
        }
      }
    } else {
      const ssw = sw.switch as SelectSwitch;
      if (ssw.name in currentSwitches) {
        const value = currentSwitches[ssw.name];
        if (
          typeof value === "string" &&
          ssw.options.find((opt): boolean => opt.name === value) !== undefined
        ) {
          state = selectFunc(ssw, value, state);
        } else {
          state = selectFunc(ssw, ssw.default, state);
        }
      } else {
        state = selectFunc(ssw, ssw.default, state);
      }
    }
  }
  return state;
}
