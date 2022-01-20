import React from "react";
import Nav from "react-bootstrap/Nav";
import Button from "react-bootstrap/Button";
import Form from "react-bootstrap/Form";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";
import { FileEarmarkText, Pencil, Check, X } from "react-bootstrap-icons";

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
        style={{ height: 40 }}
        className="py-0 pe-4px"
      >
        <div className="d-flex gap-4px h-100 align-items-center">
          {renamingSource === null || !renamingSource.renaming ? (
            <>
              <div className="d-flex gap-4px align-items-center">
                <FileEarmarkText />
                {source.filename || ""}
              </div>
              <div className="d-flex align-items-center">
                {source.filename === null || readonly || !active ? null : (
                  <>
                    <Button
                      className="px-4px"
                      variant="link"
                      onClick={(e: React.MouseEvent): void => {
                        onClickTabEdit(index);
                        e.stopPropagation();
                      }}
                    >
                      <Pencil />
                    </Button>
                    <Button
                      className="px-4px"
                      variant="link"
                      onClick={(e): void => {
                        onClickTabClose(index);
                        e.stopPropagation();
                      }}
                    >
                      <X />
                    </Button>
                  </>
                )}
              </div>
            </>
          ) : (
            <>
              <Form.Control
                type="text"
                value={renamingSource.filename}
                onClick={(e: React.MouseEvent): void => e.stopPropagation()}
                onKeyDown={(e: React.KeyboardEvent): void => {
                  if (e.key === "Enter") {
                    onSubmitRenamingFilename(index);
                  } else if (e.key === "Escape") {
                    onCancelRenamingFilename(index);
                  }
                }}
                onChange={(e): void =>
                  onChangeRenamingFilename(index, e.target.value)
                }
              />
              <div className="d-flex align-items-center">
                <Button
                  className="px-4px"
                  variant="link"
                  onClick={(): void => onCancelRenamingFilename(index)}
                >
                  <X />
                </Button>
                <Button
                  className="px-4px"
                  variant="link"
                  onClick={(): void => onSubmitRenamingFilename(index)}
                >
                  <Check />
                </Button>
              </div>
            </>
          )}
        </div>
      </Nav.Link>
    </Nav.Item>
  );
};

EditorTab.displayName = "EditorTab";

export { EditorTab };
