import React from "react";
import Nav from "react-bootstrap/Nav";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";

import { FileEarmarkText, Pencil, Check } from "~/components/atoms/icons";
import { EditorSourceData } from "~/contexts/EditorContext";

export interface RenamingSource {
  renaming: boolean;
  filename: string;
  originalFilename: string;
}

interface EditorTabProps {
  index: number;
  source: EditorSourceData;
  readonly: boolean;
  renamingSource: RenamingSource | null;
  active: boolean;
  onChangeTabs: (index: number) => void;
  onClickTabEdit: (index: number) => void;
  onClickTabClose: (index: number) => void;
  onChangeRenamingFilename: (index: number, filename: string) => void;
  onCancelRenamingFilename: (index: number) => void;
  onSubmitRenamingFilename: (index: number) => void;
}

const EditorTab: React.FC<EditorTabProps> = (props) => {
  const {
    index,
    source,
    readonly,
    renamingSource,
    active,
    onChangeTabs,
    onClickTabEdit,
    onClickTabClose,
    onChangeRenamingFilename,
    onCancelRenamingFilename,
    onSubmitRenamingFilename,
  } = props;

  return (
    <Nav.Item key={index}>
      <Nav.Link
        eventKey={`wb-editor-${index}`}
        onClick={() => onChangeTabs(index)}
      >
        {((): React.ReactElement => {
          if (renamingSource === null || !renamingSource.renaming) {
            return (
              <React.Fragment>
                <FileEarmarkText />
                {source.filename || ""}
                {source.filename === null || readonly || !active ? null : (
                  <React.Fragment>
                    <Button
                      variant="link"
                      onClick={(e: React.MouseEvent): void => {
                        onClickTabEdit(index);
                        e.stopPropagation();
                      }}
                    >
                      <Pencil />
                    </Button>
                    <button
                      type="button"
                      className="close"
                      aria-label="Close"
                      onClick={(e): void => {
                        onClickTabClose(index);
                        e.stopPropagation();
                      }}
                    >
                      <span aria-hidden="true">&times;</span>
                    </button>
                  </React.Fragment>
                )}
              </React.Fragment>
            );
          } else {
            return (
              <React.Fragment>
                <Row>
                  <Col sm="auto">
                    <Form.Control
                      type="text"
                      value={renamingSource.filename}
                      onClick={(e: React.MouseEvent): void =>
                        e.stopPropagation()
                      }
                      onChange={(e): void =>
                        onChangeRenamingFilename(index, e.target.value)
                      }
                    />
                  </Col>
                  <Col sm="auto" style={{ padding: 0 }}>
                    <Button
                      variant="link"
                      onClick={(): void => onCancelRenamingFilename(index)}
                    >
                      &times;
                    </Button>
                  </Col>
                  <Col sm="auto" style={{ padding: 0 }}>
                    <Button
                      variant="link"
                      onClick={(): void => onSubmitRenamingFilename(index)}
                    >
                      <Check />
                    </Button>
                  </Col>
                </Row>
              </React.Fragment>
            );
          }
        })()}
      </Nav.Link>
    </Nav.Item>
  );
};

EditorTab.displayName = "EditorTab";

export { EditorTab };
