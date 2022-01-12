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
    <div>
      <Row>
        <Col>
          <h6>Options</h6>
        </Col>
      </Row>
      <hr />
      {compilerInfo.switches.map(
        (sw): React.ReactElement => {
          if (sw.type === "single") {
            const ssw = sw.switch as SingleSwitch;
            // checkbox
            const checked =
              ssw.name in switches
                ? (switches[ssw.name] as boolean)
                : ssw.default;
            return (
              <Row key={ssw.name}>
                <Col>
                  <Form.Group controlId={`compileroption-single-${ssw.name}`}>
                    <Form.Check
                      type="checkbox"
                      disabled={readOnly}
                      checked={checked}
                      label={ssw.displayName}
                      onChange={(
                        e: React.ChangeEvent<HTMLInputElement>
                      ): void => onChangeChecked(ssw.name, e.target.checked)}
                    />
                  </Form.Group>
                </Col>
              </Row>
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
            return (
              <Row key={ssw.name}>
                <Col>
                  <Form.Group controlId={`compileroption-select-${ssw.name}`}>
                    <Form.Control
                      as="select"
                      disabled={readOnly}
                      value={value}
                      onChange={(
                        e: React.ChangeEvent<{
                          name?: string;
                          value: unknown;
                        }>
                      ): void =>
                        onChangeSelected(ssw.name, e.target.value as string)
                      }
                    >
                      {[...ssw.options].reverse().map(
                        (opt): React.ReactElement => {
                          return (
                            <option key={opt.name} value={opt.name}>
                              {opt.displayName}
                            </option>
                          );
                        }
                      )}
                    </Form.Control>
                  </Form.Group>
                </Col>
              </Row>
            );
          } else {
            throw "error";
          }
        }
      )}
    </div>
  );
};

export { CompilerOption };
