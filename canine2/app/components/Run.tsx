import React, { useCallback, useEffect, useState } from "react";
import { useSelector } from "react-redux";
import Button from "react-bootstrap/Button";
import { useNavigate } from "@remix-run/react";

import type {
  CompilerInfo,
  CompilerList,
  SelectSwitch,
  SingleSwitch,
} from "~/hooks/compilerList";
import type { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { useError } from "~/hooks/error";
import type { AppState } from "~/store";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import { saveHistory } from "~/features/actions";
import { useCompileStateSelector } from "~/utils/compile";
import { useTranslation } from "react-i18next";
import { OverlayTrigger, Tooltip } from "react-bootstrap";

export interface RunProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Run: React.FC<RunProps> = (props): React.ReactElement => {
  const { t } = useTranslation();
  const { compilerList, permlinkData } = props;
  const { running, navigate, history, storageExists } = useSelector(
    ({ wandbox: { running, navigate, history, storageExists } }: AppState) => ({
      running,
      navigate,
      history,
      storageExists,
    })
  );
  const compileState = useCompileStateSelector();
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const doCompile = useCompile(dispatch, compileState, compilerList);
  const doNavigate = useNavigate();
  const [, setError] = useError();

  const onRun = useCallback((): void => {
    dispatch(actions.setRunning(true));
    dispatch(actions.setSharable(true));
    dispatch(actions.prepareRun());
    doCompile();
  }, [doCompile]);

  const onEdit = useCallback(() => {
    if (permlinkData === null) {
      return null;
    }

    // ひたすら permlinkData の内容を各状態へ設定して、
    // その後それを localStorage に書き込む（ページ遷移で状態がリセットされてしまうため）。
    // コンパイラ情報は既に存在しない場合もあるので、その場合も考慮する

    const {
      compilerInfo,
      options,
      compilerOptionRaw,
      runtimeOptionRaw,
      code,
      codes,
      stdin,
      title,
      description,
    } = permlinkData.parameter;

    // CompilerContext への設定
    {
      if (!(compilerInfo.language in compilerList.languages)) {
        // 言語が見つからない場合は諦める
        setError("対象の言語は既に削除されています。");
        return;
      }
      dispatch(actions.setCurrentLanguage(compilerInfo.language));

      let currentCompilerName: string;
      if (
        compilerList.compilers.findIndex(
          (info): boolean => info.name === compilerInfo.name
        ) === -1
      ) {
        // コンパイラが見つからなかったらとりあえずその言語の先頭のコンパイラにしておく
        currentCompilerName =
          compilerList.languages[compilerInfo.language][0].name;
      } else {
        currentCompilerName = compilerInfo.name;
      }
      dispatch(actions.setCurrentCompilerName(currentCompilerName));

      // PermlinkData の存在しないかもしれないコンパイラ情報ではなく、
      // 必ず存在する方のコンパイラ情報
      const targetCompilerInfo = compilerList.compilers.find(
        (x): boolean => x.name === currentCompilerName
      ) as CompilerInfo;

      // optionList は文字列のリストなので、
      // これを頑張って {[name: string]: string | boolean} に変換する必要がある。
      const optionList = options.split(",");
      const switches: { [name: string]: boolean | string } = {};
      for (const sw of targetCompilerInfo.switches) {
        if (sw.type === "single") {
          // single の場合は、スイッチの中にオプション名が存在したら
          // スイッチ名をキーとして true を設定する
          const ssw = sw.switch as SingleSwitch;
          if (optionList.findIndex((opt): boolean => opt === ssw.name) !== -1) {
            switches[ssw.name] = true;
          }
        } else if (sw.type === "select") {
          // select の場合は、選択項目一覧の中にオプション名が存在したら
          // スイッチ名をキーとして選択項目を設定する。
          const ssw = sw.switch as SelectSwitch;
          for (const swopt of ssw.options) {
            if (
              optionList.findIndex((opt): boolean => opt === swopt.name) !== -1
            ) {
              switches[ssw.name] = swopt.name;
            }
          }
        }
      }
      dispatch(actions.setCurrentSwitches(switches));

      // raw オプションは単に設定するだけ
      dispatch(actions.setCompilerOptionRaw(compilerOptionRaw));
      dispatch(actions.setRuntimeOptionRaw(runtimeOptionRaw));
    }

    // EditorContext への設定
    {
      dispatch(actions.setTitle(title));
      dispatch(actions.setDescription(description));
      dispatch(actions.initSources(createEditorSourceData(code, codes)));
      dispatch(actions.setStdin(stdin));
    }
    // ResultContext への設定
    {
      dispatch(actions.setResults(permlinkData.results));
    }

    dispatch(actions.pushQuickSave());

    dispatch(actions.setNavigate("/"));
  }, [compilerList, permlinkData]);

  useEffect(() => {
    if (navigate.length === 0) {
      return;
    }

    saveHistory(history, storageExists);
    dispatch(actions.clearNavigate());

    doNavigate(navigate);
  }, [navigate, history, storageExists]);

  // 共有中は編集ボタン
  if (permlinkData !== null) {
    return (
      <Button style={{ minWidth: 144 }} variant="info" onClick={onEdit}>
        {t("run.edit")}
      </Button>
    );
  }

  return (
    <OverlayTrigger
      placement="bottom"
      overlay={
        <Tooltip className="d-none d-md-block" id="wb-tooltip-run">
          {t("run.ctrlEnter")}
        </Tooltip>
      }
    >
      <Button
        style={{
          minWidth: 144,
          visibility: running ? "hidden" : "visible",
        }}
        onClick={onRun}
        variant="primary"
      >
        {t("run.run")}
      </Button>
    </OverlayTrigger>
  );
};

export { Run };
