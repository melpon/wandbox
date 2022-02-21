import React from "react";
import { Form } from "react-bootstrap";

interface RawCompilerOptionProps {
  enabledCompilerOptionRaw: boolean;
  compilerOptionRaw: string;
  enabledRuntimeOptionRaw: boolean;
  runtimeOptionRaw: string;
  readOnly: boolean;
  onChangeCompilerOptionRaw: (value: string) => void;
  onChangeRuntimeOptionRaw: (value: string) => void;
  onCtrlEnter?: () => void;
}

const RawCompilerOption: React.FC<RawCompilerOptionProps> = (
  props
): React.ReactElement => {
  const {
    enabledCompilerOptionRaw,
    compilerOptionRaw,
    enabledRuntimeOptionRaw,
    runtimeOptionRaw,
    readOnly,
    onChangeCompilerOptionRaw,
    onChangeRuntimeOptionRaw,
  } = props;
  return (
    <div className="d-flex flex-column gap-16px">
      {enabledCompilerOptionRaw && (
        <div className="d-flex flex-column gap-8px">
          <h6>Raw compiler options</h6>
          <div className="px-8px d-flex flex-column gap-4px">
            {readOnly ? (
              <div style={{ whiteSpace: "pre-wrap" }}>{compilerOptionRaw}</div>
            ) : (
              <Form.Control
                as="textarea"
                value={compilerOptionRaw}
                onChange={(e) => onChangeCompilerOptionRaw(e.target.value)}
              />
            )}
          </div>
        </div>
      )}
      {enabledRuntimeOptionRaw && (
        <div className="d-flex flex-column gap-8px">
          <h6>Raw runtime options</h6>
          <div className="px-8px d-flex flex-column gap-4px">
            {readOnly ? (
              <div style={{ whiteSpace: "pre-wrap" }}>{runtimeOptionRaw}</div>
            ) : (
              <Form.Control
                as="textarea"
                value={runtimeOptionRaw}
                onChange={(e) => onChangeRuntimeOptionRaw(e.target.value)}
              />
            )}
          </div>
        </div>
      )}
    </div>
  );
};

export { RawCompilerOption };
