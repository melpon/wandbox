import React, { useEffect } from "react";
import { useContainer } from "unstated-next";
import useReactRouter from "use-react-router";
import Button from "@material-ui/core/Button";
import Grid from "@material-ui/core/Grid";
import makeStyles from "@material-ui/styles/makeStyles";

import { ResultContext } from "~/contexts/ResultContext";
import { EditorContext } from "~/contexts/EditorContext";
import { CompilerContext } from "~/contexts/CompilerContext";
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

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
const useStyles = makeStyles(() => ({
  share: {},
  edit: {},
  hidden: {
    visibility: "hidden",
  },
}));

export interface PermlinkProps {
  compilerList: CompilerList;
  permlinkData: PermlinkData | null;
  clearPermlinkData: () => void;
}

const Permlink: React.FC<PermlinkProps> = (
  props
): React.ReactElement | null => {
  const classes = useStyles();
  const { compilerList, permlinkData, clearPermlinkData } = props;
  const { history } = useReactRouter();
  const compiler = useContainer(CompilerContext);
  const editor = useContainer(EditorContext);
  const result = useContainer(ResultContext);
  const [, setError] = useError();
  const [sharing, setSharing] = React.useState<boolean>(false);

  const [permlinkResp, permlinkFetchId, doPermlink] = usePostPermlink(
    "https://wandbox.org/api/permlink",
    setError
  );

  const onShare = React.useCallback((): void => {
    setSharing(true);
    const json = createBody(editor, compiler, compilerList);
    if (json === null) {
      return;
    }
    const body = JSON.stringify({
      ...json,
      results: result.results,
      login: false,
    });

    doPermlink(null, { body: body });
  }, [compilerList, editor, compiler, result]);

  useEffect((): void => {
    // 初回での更新は弾く
    if (permlinkResp === null) {
      return;
    }

    // リクエストが完了したら共有ボタンを有効にする
    setSharing(false);

    // URL の切り替え
    history.push(`/permlink/${permlinkResp.permlink}`);
  }, [permlinkFetchId]);

  const onEdit = React.useCallback((): void => {
    if (permlinkData === null) {
      return;
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
    } = permlinkData.parameter;

    // CompilerContext への設定
    {
      if (!(compilerInfo.language in compilerList.languages)) {
        // 言語が見つからない場合は諦める
        setError("対象の言語は既に削除されています。");
        return;
      }
      compiler.setCurrentLanguage(compilerInfo.language);

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
      compiler.setCurrentCompilerName(currentCompilerName);

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
      compiler.setCurrentSwitches(switches);

      // raw オプションは単に設定するだけ
      compiler.setCompilerOptionRaw(compilerOptionRaw);
      compiler.setRuntimeOptionRaw(runtimeOptionRaw);
    }

    // EditorContext への設定
    {
      editor.setSources(createEditorSourceData(code, codes));
      editor.setStdin(stdin);
    }

    // ResultContext への設定
    {
      result.setResults(permlinkData.results);
    }

    // permlink のデータをクリアすれば全体的に編集可能になる
    clearPermlinkData();

    // URL の切り替え
    history.push(`/`);
  }, [permlinkData, compiler, editor, result]);

  const disabledShare =
    sharing || permlinkData !== null || result.results.length === 0;
  const disabledEdit = permlinkData === null;
  return (
    <Grid container spacing={2}>
      <Grid item className={disabledShare ? classes.hidden : undefined}>
        <Button
          onClick={onShare}
          disabled={disabledShare}
          variant="contained"
          color="primary"
        >
          Share
        </Button>
      </Grid>
      <Grid item className={disabledEdit ? classes.hidden : undefined}>
        <Button
          onClick={onEdit}
          disabled={disabledEdit}
          variant="contained"
          color="primary"
        >
          Edit
        </Button>
      </Grid>
    </Grid>
  );
};

export { Permlink };
