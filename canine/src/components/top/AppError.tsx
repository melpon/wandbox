import React, { useCallback } from "react";
import Toast from "react-bootstrap/Toast";
import { useError } from "~/hooks/error";

const AppError: React.FC = (props): React.ReactElement => {
  const [error, setError] = useError();

  const onClose = useCallback((): void => setError(null), []);

  return (
    <React.Fragment>
      {props.children}

      <Toast
        style={{
          position: "fixed",
          bottom: 20,
          left: 20,
          width: 300,
          zIndex: 100,
        }}
        onClose={onClose}
        show={error !== null}
        delay={3000}
        autohide
      >
        <Toast.Header>
          <strong className="mr-auto">Error</strong>
        </Toast.Header>
        <Toast.Body>{error}</Toast.Body>
      </Toast>
    </React.Fragment>
  );
};

export { AppError };
