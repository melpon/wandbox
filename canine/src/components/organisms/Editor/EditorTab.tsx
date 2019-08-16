import React from "react";
import Typography from "@material-ui/core/Typography";
import IconButton from "@material-ui/core/IconButton";
import TextField from "@material-ui/core/TextField";
import InsertDriveFileIcon from "@material-ui/icons/InsertDriveFile";
import EditIcon from "@material-ui/icons/Edit";
import ClearIcon from "@material-ui/icons/Clear";
import CheckIcon from "@material-ui/icons/Check";
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
  onClickTabEdit: (index: number) => void;
  onClickTabClose: (index: number) => void;
  onChangeRenamingFilename: (index: number, filename: string) => void;
  onCancelRenamingFilename: (index: number) => void;
  onSubmitRenamingFilename: (index: number) => void;
}

export const EditorTab = React.forwardRef<HTMLDivElement, EditorTabProps>(
  (props, ref): React.ReactElement => {
    const {
      index,
      source,
      readonly,
      renamingSource,
      onClickTabEdit,
      onClickTabClose,
      onChangeRenamingFilename,
      onCancelRenamingFilename,
      onSubmitRenamingFilename,
      ...others
    } = props;

    return (
      <div ref={ref} {...others}>
        <InsertDriveFileIcon />
        {((): React.ReactElement => {
          if (renamingSource === null || !renamingSource.renaming) {
            return (
              <React.Fragment>
                <Typography variant="body1">{source.filename || ""}</Typography>
                {source.filename === null || readonly ? null : (
                  <React.Fragment>
                    <IconButton
                      onClick={(e): void => {
                        onClickTabEdit(index);
                        e.stopPropagation();
                      }}
                    >
                      <EditIcon />
                    </IconButton>
                    <IconButton
                      onClick={(e): void => {
                        onClickTabClose(index);
                        e.stopPropagation();
                      }}
                    >
                      <ClearIcon />
                    </IconButton>
                  </React.Fragment>
                )}
              </React.Fragment>
            );
          } else {
            return (
              <React.Fragment>
                <TextField
                  label="Filename"
                  value={renamingSource.filename}
                  onClick={(e): void => e.stopPropagation()}
                  onChange={(e): void =>
                    onChangeRenamingFilename(index, e.target.value)
                  }
                />
                <ClearIcon
                  onClick={(): void => onCancelRenamingFilename(index)}
                />
                <CheckIcon
                  onClick={(): void => onSubmitRenamingFilename(index)}
                />
              </React.Fragment>
            );
          }
        })()}
      </div>
    );
  }
);

EditorTab.displayName = "EditorTab";
