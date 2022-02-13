import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import Button from "react-bootstrap/Button";

import {
  CompilerInfo,
  CompilerList,
  SelectSwitch,
  SingleSwitch,
} from "~/hooks/compilerList";
import { EditorContext, useEditorContext } from "~/contexts/EditorContext";
import { ResultContext, useResultContext } from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";
import { useCompile } from "~/hooks/compile";
import { BoxArrowUp } from "react-bootstrap-icons";
import { useNavigate } from "remix";
import { createEditorSourceData } from "~/utils/createEditorSourceData";
import { useError } from "~/hooks/error";
import { usePersistence } from "~/hooks/persistence";
import { useSidebarContext } from "~/contexts/SidebarContext";
import { AppState, useAppDispatch, useAppStore } from "~/store";
import { wandboxSlice } from "~/features/slice";

export interface RunProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
}

const Run: React.FC<RunProps> = (props): React.ReactElement => {
  const { compilerList, permlinkData } = props;
  const editor = useEditorContext();
  const state = useSelector(({ wandbox }: AppState) => wandbox);
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const result = useResultContext();
  const sidebar = useSidebarContext();
  const doCompile = useCompile(editor, state, sidebar, compilerList, result);
  const navigate = useNavigate();
  const [, setError] = useError();
  const { save } = usePersistence(
    editor,
    dispatch,
    state,
    result,
    sidebar,
    false
  );
  const [editCompleted, setEditCompleted] = useState(false);

  const onRun = React.useCallback((): void => {
    editor.setRunning(true);
    editor.setSharable(true);
    sidebar.history.prepareRun(state, editor);
    doCompile();
  }, [state, editor, sidebar, doCompile]);

  //const disabled = permlinkData !== null || currentCompilerName === "";

  const onEdit = React.useCallback(() => {
    if (permlinkData === null) {
      return null;
    }

    // ひたすら permlinkData の内容を各状態へ設定していく
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
      editor.setTitle(title);
      editor.setDescription(description);
      editor.initSources(createEditorSourceData(code, codes));
      editor.setStdin(stdin);
    }
    // ResultContext への設定
    {
      result.setResults(permlinkData.results);
    }

    // すぐに save() すると値が反映されないので、
    // editCompleted の値が変化するのを待ってから save() する
    setEditCompleted(true);
  }, []);

  useEffect(() => {
    if (!editCompleted) {
      return;
    }

    // この後 navigate で遷移すると状態がリセットされてしまうため
    // localStorage に保存しておいて遷移後に読み込ませる
    save();

    navigate("/");
  }, [editCompleted]);

  // 共有中は編集ボタン
  if (permlinkData !== null) {
    return (
      <Button style={{ minWidth: 144 }} variant="info" onClick={onEdit}>
        Edit
      </Button>
    );
  }

  // 実行中は表示しない
  return (
    <Button
      style={{
        minWidth: 144,
        visibility: editor.running ? "hidden" : "visible",
      }}
      onClick={onRun}
      variant="primary"
    >
      Run
    </Button>
  );
};

export { Run };
