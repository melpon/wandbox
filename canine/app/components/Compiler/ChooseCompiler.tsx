import React from "react";
import { Dropdown } from "react-bootstrap";
import { ThreeDots } from "react-bootstrap-icons";
import ListGroup from "react-bootstrap/ListGroup";

import { CompilerInfo } from "~/hooks/compilerList";

interface ChooseCompilerProps {
  compilerInfo: CompilerInfo | null;
  compilerInfos: CompilerInfo[];
  readOnly: boolean;
  templates: string[];
  onSelectCompiler: (compiler: CompilerInfo) => void;
  onDeselectCompiler: () => void;
  onLoadTemplate: (template: string) => void;
}

const ChooseCompiler: React.FC<ChooseCompilerProps> = (
  props
): React.ReactElement => {
  const {
    compilerInfo,
    compilerInfos,
    readOnly,
    templates,
    onSelectCompiler,
    onDeselectCompiler,
    onLoadTemplate,
  } = props;

  return compilerInfo === null ? (
    <ListGroup className="wb-compilerlist">
      <ListGroup.Item>Compiler</ListGroup.Item>
      {compilerInfos.map((info): React.ReactElement => {
        return (
          <ListGroup.Item
            className="wb-item"
            key={info.name}
            action
            onClick={(): void => onSelectCompiler(info)}
          >
            {`${info.displayName} ${info.version}`}
          </ListGroup.Item>
        );
      })}
    </ListGroup>
  ) : (
    <div className="d-flex flex-column gap-8px wb-compilerlist-selected">
      <div className="d-flex justify-content-between">
        <h6>Compiler</h6>
        {!readOnly && (
          <Dropdown className="wb-loadtemplate" align="end">
            <Dropdown.Toggle variant="link">
              <ThreeDots />
            </Dropdown.Toggle>
            <Dropdown.Menu>
              {templates.map((template) => {
                return (
                  <Dropdown.Item
                    key={template}
                    onClick={() => onLoadTemplate(template)}
                  >
                    Load template
                  </Dropdown.Item>
                );
              })}
            </Dropdown.Menu>
          </Dropdown>
        )}
      </div>
      <div className="px-8px d-flex justify-content-between">
        <p>{`${compilerInfo.displayName} ${compilerInfo.version}`}</p>
        {readOnly ? null : (
          <button
            type="button"
            className="btn-close"
            aria-label="Close"
            onClick={onDeselectCompiler}
          />
        )}
      </div>
    </div>
  );
  /*
  return (
    <ListGroup className="wb-compilerlist">
      <ListGroup.Item>Compiler</ListGroup.Item>
      {compilerInfo === null ? (
        compilerInfos.map(
          (info): React.ReactElement => {
            return (
              <ListGroup.Item
                className="wb-item"
                key={info.name}
                action
                onClick={(): void => onSelectCompiler(info)}
              >
                <p>{`${info.displayName} ${info.version}`}</p>
              </ListGroup.Item>
            );
          }
        )
      ) : (
        <ListGroup.Item className="wb-item">
          <p>{`${compilerInfo.displayName} ${compilerInfo.version}`}</p>
          {readOnly ? null : (
            <button
              type="button"
              className="close"
              aria-label="Close"
              onClick={onDeselectCompiler}
            >
              <span aria-hidden="true">&times;</span>
            </button>
          )}
        </ListGroup.Item>
      )}
    </ListGroup>
  );
  */
};

export { ChooseCompiler };
