import React from "react";
import { Button } from "react-bootstrap";
import { useSelector } from "react-redux";
import { AppState, useAppDispatch } from "~/store";
import { formatDistanceToNow } from "date-fns";
import { wandboxSlice } from "~/features/slice";
import { useNavigate, useParams } from "remix";

const History: React.FC = () => {
  const { permlinkId } = useParams();
  const { history } = useSelector(({ wandbox: { history } }: AppState) => ({
    history,
  }));
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const navigate = useNavigate();

  return (
    <div className="wb-history flex-grow-1 d-flex flex-column px-16px py-8px gap-8px">
      {[...history.histories].reverse().map((x, i) => (
        <div
          key={`wb-history-${x.id}`}
          className="wb-history-data d-flex flex-column px-8px py-8px"
        >
          <div className="d-flex justify-content-between">
            <p className="wb-weak-text">
              {formatDistanceToNow(x.createdAt * 1000, {
                addSuffix: true,
              })}
            </p>
            {x.type === "permlink" && (
              <p className="wb-weak-text">{`${x.permlinkId}`}</p>
            )}
          </div>
          <div className="d-flex">
            <p className={`wb-title wb-${x.type}`}>{x.title}</p>
          </div>
          <div
            className={`d-flex justify-content-between px-8px pt-8px ${
              x.title.length === 0 ? "mt-8px" : ""
            }`}
          >
            <div className="d-flex flex-column justify-content-center">
              <p>
                {x.currentLanguage} {x.currentCompilerName}
              </p>
              {x.type === "permlink" && (
                <>
                  <p className="wb-weak-text">
                    Created at{" "}
                    {formatDistanceToNow(x.permlinkCreatedAt * 1000, {
                      addSuffix: true,
                    })}
                  </p>
                  {x.githubUser && (
                    <p className="wb-weak-text">{`@${x.githubUser.login}`}</p>
                  )}
                  {!x.githubUser && (
                    <p className="wb-weak-text">Created by anonymous</p>
                  )}
                </>
              )}
            </div>
            <div className="wb-buttons d-flex align-items-center">
              {x.type === "permlink" ? (
                <Button
                  variant="info"
                  onClick={() => {
                    if (permlinkId === undefined) {
                      dispatch(actions.pushQuickSave());
                    }
                    navigate(`/permlink/${x.permlinkId}`);
                  }}
                >
                  View
                </Button>
              ) : (
                <Button
                  variant="primary"
                  onClick={() => {
                    if (permlinkId !== undefined) {
                      navigate(`/`);
                    }
                    if (permlinkId === undefined) {
                      dispatch(actions.pushQuickSave());
                    }
                    dispatch(actions.loadQuickSave(x));
                  }}
                >
                  Load
                </Button>
              )}
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};
export { History };
