import {
  CompilerInfo,
  SelectSwitch,
  SelectSwitchOption,
  SingleSwitch,
} from "~/hooks/compilerList";

// カンマ区切りのコンパイラオプションの文字列から、
// {[name: string]: string | boolean} のコンパイラオプション情報に変換する
export function optionsToSwitch(
  optionsStr: string,
  compilerInfo: CompilerInfo
): { [name: string]: string | boolean } {
  const options = optionsStr.split(",");
  const switches: { [name: string]: string | boolean } = {};
  compilerInfo.switches.forEach((sw): void => {
    if (sw.type === "single") {
      const ssw = sw.switch as SingleSwitch;
      // checkbox
      const checked = options.findIndex((x): boolean => x === ssw.name) !== -1;
      switches[ssw.name] = checked;
    } else if (sw.type === "select") {
      const ssw = sw.switch as SelectSwitch;
      // select
      const value = ((): SelectSwitchOption | null => {
        // ssw.options の中から options に含まれるオプションを探す。
        // 多分複数一致することは無いはずだし、複数あってもどうしようも無いので
        // 最初に一致したものを返す。
        for (const opt of ssw.options) {
          for (const target of options) {
            if (opt.name === target) {
              return opt;
            }
          }
        }

        return null;
      })();
      if (value !== null) {
        switches[ssw.name] = value.name;
      }
    } else {
      throw "error";
    }
  });
  return switches;
}
