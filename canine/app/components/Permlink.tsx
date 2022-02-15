import React, { useEffect, useRef, useState } from "react";
import { useSelector } from "react-redux";
import Button from "react-bootstrap/Button";

import { createBody } from "~/utils/compile";
import {
  CompilerList,
  CompilerInfo,
  SingleSwitch,
  SelectSwitch,
} from "~/hooks/compilerList";
import { useError } from "~/hooks/error";
import { usePostPermlink, PermlinkData } from "~/hooks/permlink";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { useNavigate } from "remix";
import { BoxArrowUp, DistributeHorizontal } from "react-bootstrap-icons";
import { AppState, useAppDispatch, useAppStore } from "~/store";
import { wandboxSlice } from "~/features/slice";

interface TweetButtonProps {
  permlinkId: string;
}

const TweetButton: React.FC<TweetButtonProps> = ({ permlinkId }) => {
  const [ref, setRef] = useState<HTMLDivElement | null>(null);
  useEffect((): void => {
    if (ref === null) {
      return;
    }
    const url = `${document.location.origin}/permlink/${permlinkId}`;
    twttr.ready((twttr) => {
      twttr.widgets.createShareButton(url, ref, {});
    });
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
  clearPermlinkData: () => void;
}

const Permlink: React.FC<PermlinkProps> = (
  props
): React.ReactElement | null => {
  const { compilerList, permlinkData, clearPermlinkData } = props;
  const navigate = useNavigate();
  const state = useSelector(({ wandbox }: AppState) => wandbox);
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const [, setError] = useError();
  const [sharing, setSharing] = React.useState<boolean>(false);

  const [permlinkResp, permlinkFetchId, doPermlink] = usePostPermlink(
    `${WANDBOX_URL_PREFIX}/api/permlink`,
    setError
  );

  const onShare = React.useCallback((): void => {
    setSharing(true);
    const json = createBody(state, compilerList);
    if (json === null) {
      return;
    }
    const body = JSON.stringify({
      ...json,
      results: state.results,
      login: false,
    });

    doPermlink(null, { body: body });
  }, [compilerList, state]);

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
  }, [state.sources]);

  //const onEdit = React.useCallback((): void => {
  //  if (permlinkData === null) {
  //    return;
  //  }
  //  // ひたすら permlinkData の内容を各状態へ設定していく
  //  // コンパイラ情報は既に存在しない場合もあるので、その場合も考慮する

  //  const {
  //    compilerInfo,
  //    options,
  //    compilerOptionRaw,
  //    runtimeOptionRaw,
  //    code,
  //    codes,
  //    stdin,
  //  } = permlinkData.parameter;

  //  // CompilerContext への設定
  //  {
  //    if (!(compilerInfo.language in compilerList.languages)) {
  //      // 言語が見つからない場合は諦める
  //      setError("対象の言語は既に削除されています。");
  //      return;
  //    }
  //    compiler.setCurrentLanguage(compilerInfo.language);

  //    let currentCompilerName: string;
  //    if (
  //      compilerList.compilers.findIndex(
  //        (info): boolean => info.name === compilerInfo.name
  //      ) === -1
  //    ) {
  //      // コンパイラが見つからなかったらとりあえずその言語の先頭のコンパイラにしておく
  //      currentCompilerName =
  //        compilerList.languages[compilerInfo.language][0].name;
  //    } else {
  //      currentCompilerName = compilerInfo.name;
  //    }
  //    compiler.setCurrentCompilerName(currentCompilerName);

  //    // PermlinkData の存在しないかもしれないコンパイラ情報ではなく、
  //    // 必ず存在する方のコンパイラ情報
  //    const targetCompilerInfo = compilerList.compilers.find(
  //      (x): boolean => x.name === currentCompilerName
  //    ) as CompilerInfo;

  //    // optionList は文字列のリストなので、
  //    // これを頑張って {[name: string]: string | boolean} に変換する必要がある。
  //    const optionList = options.split(",");
  //    const switches: { [name: string]: boolean | string } = {};
  //    for (const sw of targetCompilerInfo.switches) {
  //      if (sw.type === "single") {
  //        // single の場合は、スイッチの中にオプション名が存在したら
  //        // スイッチ名をキーとして true を設定する
  //        const ssw = sw.switch as SingleSwitch;
  //        if (optionList.findIndex((opt): boolean => opt === ssw.name) !== -1) {
  //          switches[ssw.name] = true;
  //        }
  //      } else if (sw.type === "select") {
  //        // select の場合は、選択項目一覧の中にオプション名が存在したら
  //        // スイッチ名をキーとして選択項目を設定する。
  //        const ssw = sw.switch as SelectSwitch;
  //        for (const swopt of ssw.options) {
  //          if (
  //            optionList.findIndex((opt): boolean => opt === swopt.name) !== -1
  //          ) {
  //            switches[ssw.name] = swopt.name;
  //          }
  //        }
  //      }
  //    }
  //    compiler.setCurrentSwitches(switches);

  //    // raw オプションは単に設定するだけ
  //    compiler.setCompilerOptionRaw(compilerOptionRaw);
  //    compiler.setRuntimeOptionRaw(runtimeOptionRaw);
  //  }

  //  // EditorContext への設定
  //  {
  //    editor.setSources(createEditorSourceData(code, codes));
  //    editor.setStdin(stdin);
  //  }

  //  // ResultContext への設定
  //  {
  //    result.setResults(permlinkData.results);
  //  }

  //  // permlink のデータをクリアすれば全体的に編集可能になる
  //  clearPermlinkData();

  //  // URL の切り替え
  //  history.push(`/`);
  //}, [permlinkData, compiler, editor, result]);

  //const disabledShare =
  //  sharing || permlinkData !== null || result.results.length === 0;
  //const disabledEdit = permlinkData === null;

  return (
    <div className="d-flex">
      {!state.running && state.sharable && permlinkData === null && !sharing && (
        <Button
          className="d-flex justify-content-center align-items-center"
          style={{ minWidth: 144, fontWeight: 700 }}
          onClick={onShare}
          variant="outline-primary"
        >
          <BoxArrowUp />
          <span>Share</span>
        </Button>
      )}
      {permlinkData !== null && (
        <TweetButton permlinkId={permlinkData.permlinkId} />
      )}
    </div>
  );
};

export { Permlink };
