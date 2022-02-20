import React, { useEffect } from "react";
import { useSelector } from "react-redux";
import { Button, Form, Modal } from "react-bootstrap";
import { Pencil } from "react-bootstrap-icons";
import { useParams } from "remix";
import { wandboxSlice } from "~/features/slice";
import { PermlinkData } from "~/hooks/permlink";
import { AppState, useAppDispatch } from "~/store";

const TitleDialog: React.FC = () => {
  const { title, description, titleDialogOpened } = useSelector(
    ({ wandbox: { title, description, titleDialogOpened } }: AppState) => ({
      title,
      description,
      titleDialogOpened,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [editingTitle, setEditingTitle] = React.useState<string>("");
  const [editingDescription, setEditingDescription] =
    React.useState<string>("");

  useEffect(() => {
    if (!titleDialogOpened) {
      return;
    }

    setEditingTitle(title);
    setEditingDescription(description);
  }, [titleDialogOpened]);

  return (
    <Modal
      show={titleDialogOpened}
      onHide={() => dispatch(actions.setTitleDialogOpened(false))}
    >
      <Modal.Body className="d-flex flex-column">
        <button
          type="button"
          className="justify-content-start align-self-end btn-close"
          data-bs-dismiss="modal"
          aria-label="Close"
          onClick={() => dispatch(actions.setTitleDialogOpened(false))}
        />
        <div className="d-flex flex-column gap-16px">
          <div className="d-flex flex-column gap-4px">
            <label>Title</label>
            <Form.Control
              type="input"
              placeholder="Title"
              value={editingTitle}
              onChange={(e) => setEditingTitle(e.currentTarget.value)}
            />
          </div>
          <div className="d-flex flex-column gap-4px">
            <label>Description</label>
            <Form.Control
              as="textarea"
              rows={5}
              placeholder="Description"
              value={editingDescription}
              onChange={(e) => setEditingDescription(e.currentTarget.value)}
            />
          </div>
        </div>
      </Modal.Body>
      <Modal.Footer>
        <Button
          variant="outline-primary"
          onClick={() => dispatch(actions.setTitleDialogOpened(false))}
        >
          Cancel
        </Button>
        <Button
          variant="primary"
          onClick={() => {
            dispatch(actions.setTitle(editingTitle));
            dispatch(actions.setDescription(editingDescription));
            dispatch(actions.setTitleDialogOpened(false));
          }}
        >
          OK
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

interface TitleProps {
  permlinkData: PermlinkData | null;
}

const Title: React.FC<TitleProps> = (props) => {
  const { permlinkData } = props;
  const { title, description } = useSelector(
    ({ wandbox: { title, description } }: AppState) => ({ title, description })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  return (
    <div className="flex-grow-1 d-flex flex-column gap-8px">
      <div className="d-flex justify-content-between">
        <div className="d-flex align-items-center">
          <h4
            className={`${permlinkData !== null ? "text-info" : ""}`}
            style={{ wordBreak: "break-word" }}
          >
            {permlinkData === null ? title : permlinkData.parameter.title}
          </h4>
        </div>
        {permlinkData === null && (
          <Button
            variant="link"
            onClick={() => {
              dispatch(actions.setTitleDialogOpened(true));
            }}
          >
            <Pencil />
          </Button>
        )}
      </div>
      <div style={{ borderBottom: "#d0d7de solid 1px" }}></div>
      <p style={{ whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
        {permlinkData === null
          ? description
          : permlinkData.parameter.description}
      </p>
    </div>
  );
};

export { TitleDialog, Title };
