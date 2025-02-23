import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import { Button, Form, Modal } from "react-bootstrap";
import { Pencil } from "react-bootstrap-icons";
import { useTranslation } from "react-i18next";

import { wandboxSlice } from "~/features/slice";
import type { PermlinkData } from "~/hooks/permlink";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";

const TitleDialog: React.FC = () => {
  const { t } = useTranslation();
  const { title, description, titleDialogOpened } = useSelector(
    ({ wandbox: { title, description, titleDialogOpened } }: AppState) => ({
      title,
      description,
      titleDialogOpened,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [editingTitle, setEditingTitle] = useState<string>("");
  const [editingDescription, setEditingDescription] = useState<string>("");

  useEffect(() => {
    if (!titleDialogOpened) {
      return;
    }

    setEditingTitle(title);
    setEditingDescription(description);
  }, [titleDialogOpened, title, description]);

  useEffect(() => {
    setInvalidTitle([...editingTitle].length > 100);
    setInvalidDescription([...editingDescription].length > 1000);
  }, [editingTitle, editingDescription]);
  const [invalidTitle, setInvalidTitle] = useState(false);
  const [invalidDescription, setInvalidDescription] = useState(false);

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
            <label>{t("title.title")}</label>
            <Form.Control
              type="input"
              placeholder="Title"
              value={editingTitle}
              onChange={(e) => setEditingTitle(e.currentTarget.value)}
            />
            {invalidTitle && (
              <p className="wb-control-error">Enter within 100 characters.</p>
            )}
          </div>
          <div className="d-flex flex-column gap-4px">
            <label>{t("title.description")}</label>
            <Form.Control
              as="textarea"
              rows={5}
              placeholder="Description"
              value={editingDescription}
              onChange={(e) => setEditingDescription(e.currentTarget.value)}
            />
            {invalidDescription && (
              <p className="wb-control-error">Enter within 1,000 characters.</p>
            )}
          </div>
        </div>
      </Modal.Body>
      <Modal.Footer>
        <Button
          variant="outline-primary"
          onClick={() => dispatch(actions.setTitleDialogOpened(false))}
        >
          {t("cancel")}
        </Button>
        <Button
          variant="primary"
          form="titleEditDialog"
          onClick={() => {
            if (invalidTitle || invalidDescription) {
              return;
            }
            dispatch(actions.setTitle(editingTitle));
            dispatch(actions.setDescription(editingDescription));
            dispatch(actions.setTitleDialogOpened(false));
          }}
        >
          {t("ok")}
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
      {(permlinkData === null || permlinkData.parameter.title.length !== 0) && (
        <div className="wb-line-horizontal"></div>
      )}
      <p style={{ whiteSpace: "pre-wrap", wordBreak: "break-word" }}>
        {permlinkData === null
          ? description
          : permlinkData.parameter.description}
      </p>
    </div>
  );
};

export { TitleDialog, Title };
