import React from "react";
import { useSelector } from "react-redux";
import { Button, OverlayTrigger, Tooltip } from "react-bootstrap";
import { formatDistanceToNow, formatRFC3339 } from "date-fns";
import { useNavigate, useParams } from "remix";

import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import { Trans, useTranslation } from "react-i18next";
import { getDateFnsLocale } from "~/utils/getDateFnsLocale";
import { t } from "i18next";

const History: React.FC = () => {
  const { i18n } = useTranslation();
  const { permlinkId } = useParams();
  const { history, breakpoint } = useSelector(
    ({ wandbox: { history, breakpoint } }: AppState) => ({
      history,
      breakpoint,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const navigate = useNavigate();
  const locale = getDateFnsLocale(i18n.language);

  return (
    <div className="wb-history flex-grow-1 d-flex flex-column px-16px py-8px gap-8px">
      {[...history.histories].reverse().map((x, i) => {
        return (
          <div
            key={`wb-history-${x.id}`}
            className="wb-history-data d-flex flex-column px-8px py-8px"
          >
            <div className="d-flex justify-content-between">
              <OverlayTrigger
                placement="top"
                overlay={
                  <Tooltip id={`wb-tooltip-history-${x.id}`}>
                    {formatRFC3339(x.createdAt * 1000)}
                  </Tooltip>
                }
              >
                <p className="wb-weak-text">
                  {t(
                    x.type === "permlink"
                      ? "history.viewedAt"
                      : "history.ranAt",
                    {
                      time: formatDistanceToNow(x.createdAt * 1000, {
                        addSuffix: true,
                        locale: locale,
                      }),
                    }
                  )}
                </p>
              </OverlayTrigger>
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
                  {x.currentLanguage} {x.displayName} {x.version}
                </p>
                {x.type === "permlink" && (
                  <>
                    <OverlayTrigger
                      placement="top"
                      overlay={
                        <Tooltip id={`wb-tooltip-history-permlink-${x.id}`}>
                          {formatRFC3339(x.permlinkCreatedAt * 1000)}
                        </Tooltip>
                      }
                    >
                      <p className="wb-weak-text">
                        {t("history.createdAt", {
                          time: formatDistanceToNow(
                            x.permlinkCreatedAt * 1000,
                            {
                              addSuffix: true,
                              locale: locale,
                            }
                          ),
                        })}
                      </p>
                    </OverlayTrigger>
                    {x.githubUser && (
                      <p className="wb-weak-text">
                        <Trans
                          i18nKey="history.createdByUser"
                          values={{ user: x.githubUser.login }}
                          components={{
                            a: (
                              <a
                                href={x.githubUser.html_url}
                                target="_blank"
                                rel="noopener noreferrer"
                              />
                            ),
                          }}
                        ></Trans>
                      </p>
                    )}
                    {!x.githubUser && (
                      <p className="wb-weak-text">
                        {t("history.createdByAnonymous")}
                      </p>
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
                      // スマホではロードしたら画面を閉じる
                      if (
                        breakpoint === "xs" ||
                        breakpoint === "sm" ||
                        breakpoint === "md"
                      ) {
                        dispatch(actions.setSidebarState("none"));
                        dispatch(actions.setSidebarLocked(false));
                      }
                    }}
                  >
                    {t("history.view")}
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
                      // スマホではロードしたら画面を閉じる
                      if (
                        breakpoint === "xs" ||
                        breakpoint === "sm" ||
                        breakpoint === "md"
                      ) {
                        dispatch(actions.setSidebarState("none"));
                        dispatch(actions.setSidebarLocked(false));
                      }
                    }}
                  >
                    {t("history.load")}
                  </Button>
                )}
              </div>
            </div>
          </div>
        );
      })}
    </div>
  );
};
export { History };
