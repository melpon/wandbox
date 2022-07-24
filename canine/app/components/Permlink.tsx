import React, { useCallback, useEffect, useState } from "react";
import { useSelector } from "react-redux";
import Button from "react-bootstrap/Button";
import { BoxArrowUp } from "react-bootstrap-icons";
import { useNavigate } from "remix";

import { createBody, useCompileStateSelector } from "~/utils/compile";
import type { CompilerList } from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { usePostPermlink, PermlinkData } from "~/hooks/permlink";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import { useTranslation } from "react-i18next";

interface TweetButtonProps {
  permlinkId: string;
}

const TweetButton: React.FC<TweetButtonProps> = ({ permlinkId }) => {
  const [ref, setRef] = useState<HTMLDivElement | null>(null);
  useEffect(() => {
    if (ref === null) {
      return;
    }
    const url = `${document.location.origin}/permlink/${permlinkId}`;
    let elem: HTMLElement | null = null;
    if (typeof twttr !== "undefined") {
      twttr.ready((twttr) => {
        twttr.widgets.createShareButton(url, ref, {}).then((x) => {
          elem = x;
        });
      });
    }

    return () => {
      if (elem === null) {
        return;
      }
      elem.remove();
    };
  }, [permlinkId, ref]);

  return (
    <div
      className="align-self-start"
      ref={setRef}
      style={{ width: 144, height: 35 }}
    />
  );
};

export interface PermlinkProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Permlink: React.FC<PermlinkProps> = (
  props
): React.ReactElement | null => {
  const { compilerList, permlinkData } = props;
  const { t } = useTranslation();
  const navigate = useNavigate();
  const compileState = useCompileStateSelector();
  const { results, running, sharable } = useSelector(
    ({ wandbox: { results, running, sharable } }: AppState) => ({
      results,
      running,
      sharable,
    })
  );
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [, setError] = useError();
  const [sharing, setSharing] = useState<boolean>(false);

  const [permlinkResp, permlinkFetchId, doPermlink] = usePostPermlink(
    "/api/permlink",
    setError
  );

  const onShare = useCallback((): void => {
    setSharing(true);
    const json = createBody(compileState, compilerList);
    if (json === null) {
      return;
    }
    const body = JSON.stringify({
      ...json,
      results: results,
    });

    doPermlink(null, { body: body });
  }, [compilerList, compileState, results]);

  useEffect((): void => {
    // 初回での更新は弾く
    if (permlinkResp === null) {
      return;
    }

    setSharing(false);

    // URL の切り替え
    const url = `/permlink/${permlinkResp.permlink}`;
    navigate(url);
  }, [permlinkFetchId]);

  // コンパイラやエディタの状態が書き換わったら共有不可にする
  useEffect(() => {
    dispatch(actions.setSharable(false));
  }, [compileState]);

  return (
    <div className="d-flex">
      {!running && sharable && permlinkData === null && !sharing && (
        <Button
          className="d-flex justify-content-center align-items-center"
          style={{ minWidth: 144, fontWeight: 700 }}
          onClick={onShare}
          variant="outline-primary"
        >
          <BoxArrowUp />
          <span>{t("permlink.share")}</span>
        </Button>
      )}
      {permlinkData !== null && (
        <TweetButton permlinkId={permlinkData.permlinkId} />
      )}
    </div>
  );
};

export { Permlink };
