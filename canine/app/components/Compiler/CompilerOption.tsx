import React from "react";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";
import Form from "react-bootstrap/Form";

import { CompilerInfo, SingleSwitch, SelectSwitch } from "~/hooks/compilerList";

interface CompilerOptionProps {
  switches: { [name: string]: string | boolean };
  compilerInfo: CompilerInfo;
  readOnly: boolean;
  onChangeChecked: (switchName: string, checked: boolean) => void;
  onChangeSelected: (switchName: string, selected: string) => void;
}

const CompilerOption: React.FC<CompilerOptionProps> = (
  props
): React.ReactElement => {
  const {
    switches,
    compilerInfo,
    readOnly,
    onChangeChecked,
    onChangeSelected,
  } = props;

  return (
    <div className="d-flex flex-column gap-8px">
      <h6>Options</h6>
      <div className="px-8px d-flex flex-column gap-4px">
        {compilerInfo.switches.map((sw): React.ReactElement | null => {
          if (sw.type === "single") {
            const ssw = sw.switch as SingleSwitch;
            // checkbox
            const checked =
              ssw.name in switches
                ? (switches[ssw.name] as boolean)
                : ssw.default;
            if (readOnly) {
              if (!checked) {
                return (
                  //<div key={ssw.name} className="wb-disabled">
                  //  {ssw.displayName}
                  //</div>
                  null
                );
              }
              return <div key={ssw.name}>{ssw.displayName}</div>;
            }

            return (
              <Form.Group
                key={ssw.name}
                controlId={`compileroption-single-${ssw.name}`}
              >
                <Form.Check
                  type="checkbox"
                  readOnly={readOnly}
                  checked={checked}
                  label={ssw.displayName}
                  onChange={(e: React.ChangeEvent<HTMLInputElement>): void => {
                    if (readOnly) {
                      return;
                    }
                    onChangeChecked(ssw.name, e.target.checked);
                  }}
                />
              </Form.Group>
            );
          } else if (sw.type === "select") {
            const ssw = sw.switch as SelectSwitch;
            // select
            const value = ((): string => {
              if (!(ssw.name in switches)) {
                return ssw.default;
              }
              const name = switches[ssw.name];
              if (typeof name !== "string") {
                return ssw.default;
              }
              if (
                ssw.options.find((opt): boolean => opt.name === name) ===
                undefined
              ) {
                return ssw.default;
              }
              return name;
            })();

            if (readOnly) {
              const opt = ssw.options.find((opt) => opt.name === value);
              return <div key={ssw.name}>{opt!.displayName}</div>;
            }

            return (
              <Form.Group
                key={ssw.name}
                controlId={`compileroption-select-${ssw.name}`}
              >
                <Form.Control
                  as="select"
                  readOnly={readOnly}
                  value={value}
                  onChange={(
                    e: React.ChangeEvent<{
                      name?: string;
                      value: unknown;
                    }>
                  ): void => {
                    if (readOnly) {
                      return;
                    }
                    onChangeSelected(ssw.name, e.target.value as string);
                  }}
                >
                  {[...ssw.options].reverse().map((opt): React.ReactElement => {
                    return (
                      <option key={opt.name} value={opt.name}>
                        {opt.displayName}
                      </option>
                    );
                  })}
                </Form.Control>
              </Form.Group>
            );
          } else {
            throw "error";
          }
        })}
      </div>
    </div>
  );
};

export { CompilerOption };
