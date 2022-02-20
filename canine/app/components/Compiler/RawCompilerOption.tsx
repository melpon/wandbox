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
  //const {
  //  compilerOptionRaw,
  //  runtimeOptionRaw,
  //  readOnly,
  //  onChangeCompilerOptionRaw,
  //  onChangeRuntimeOptionRaw,
  //  onCtrlEnter,
  //} = props;
  //const [expanded, setExpanded] = React.useState<boolean>(false);
  //const onExpandRuntimeOptionRaw = React.useCallback((): void => {
  //  setExpanded(true);
  //}, []);

  //return (
  //  <div style={{ backgroundColor: "#fff" }}>
  //    <Row>
  //      <Col>
  //        <h6>Raw Options</h6>
  //      </Col>
  //    </Row>
  //    <hr />

  //    <Row>
  //      <Col>
  //        {compilerOptionRaw === null ? null : (
  //          <>
  //            <h6>Compiler Option:</h6>
  //            <CodeMirror
  //              value={compilerOptionRaw}
  //              options={{
  //                readOnly: readOnly,
  //                viewportMargin: Infinity,
  //                smartIndent: false,
  //                extraKeys: {
  //                  "Ctrl-Enter": (): void => {
  //                    onCtrlEnter();
  //                  },
  //                },
  //              }}
  //              onBeforeChange={onChangeCompilerOptionRaw}
  //              className="wb-input"
  //            />
  //          </>
  //        )}
  //      </Col>
  //    </Row>
  //    <Row>
  //      <Col>
  //        {runtimeOptionRaw === null && !expanded ? (
  //          readOnly ? null : (
  //            <Button variant="light" onClick={onExpandRuntimeOptionRaw}>
  //              Runtime options...
  //            </Button>
  //          )
  //        ) : (
  //          <>
  //            <h6>Runtime Option:</h6>
  //            <CodeMirror
  //              value={runtimeOptionRaw === null ? "" : runtimeOptionRaw}
  //              options={{
  //                readOnly: readOnly,
  //                viewportMargin: Infinity,
  //                smartIndent: false,
  //                extraKeys: {
  //                  "Ctrl-Enter": (): void => {
  //                    onCtrlEnter();
  //                  },
  //                },
  //              }}
  //              onBeforeChange={onChangeRuntimeOptionRaw}
  //              className="wb-input"
  //            />
  //          </>
  //        )}
  //      </Col>
  //    </Row>
  //  </div>
  //);
};

export { RawCompilerOption };
