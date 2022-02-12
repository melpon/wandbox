import React from "react";
import Button from "react-bootstrap/Button";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { CodeMirror } from "~/components/organisms/CodeMirror";

interface RawCompilerOptionProps {
  compilerOptionRaw: string | null;
  runtimeOptionRaw: string | null;
  readOnly: boolean;
  onChangeCompilerOptionRaw: (
    cm: unknown,
    data: unknown,
    value: string
  ) => void;
  onChangeRuntimeOptionRaw: (cm: unknown, data: unknown, value: string) => void;
  onCtrlEnter: () => void;
}

const RawCompilerOption: React.FC<RawCompilerOptionProps> = (
  props
): React.ReactElement => {
  const { readOnly } = props;
  return (
    <div className="d-flex flex-column gap-8px">
      <h6>Raw Options</h6>
      <div className="px-8px d-flex flex-column gap-4px">
        {readOnly ? (
          <div style={{ whiteSpace: "pre-wrap" }}></div>
        ) : (
          <textarea></textarea>
        )}
      </div>
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
