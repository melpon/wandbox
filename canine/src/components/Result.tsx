import React from "react";

import {
  ResultContext,
  ResultData,
  useResultContext,
} from "~/contexts/ResultContext";
import { PermlinkData } from "~/hooks/permlink";

interface ResultProps {
  permlinkData: PermlinkData | null;
}

const Result: React.FC<ResultProps> = (props): React.ReactElement => {
  const { permlinkData } = props;
  const rs = useResultContext();
  const results = permlinkData === null ? rs.results : permlinkData.results;
  const mergedResults: ResultData[] = [];
  let preview: ResultData | null = null;
  for (const r of results) {
    const isMessage =
      r.type === "CompilerMessageS" ||
      r.type === "CompilerMessageE" ||
      r.type === "StdOut" ||
      r.type === "StdErr";

    // 直前と同じメッセージタイプなら結合する
    if (isMessage && preview !== null && preview.type === r.type) {
      mergedResults[mergedResults.length - 1] = {
        type: r.type,
        data: preview.data + r.data,
      };
    } else {
      mergedResults.push(r);
      preview = r;
    }
  }
  const typeClassNames = React.useMemo(
    // eslint-disable-next-line @typescript-eslint/explicit-function-return-type
    () => ({
      CompilerMessageS: "wb-result-compilermessages",
      CompilerMessageE: "wb-result-compilermessagee",
      StdOut: "wb-result-stdout",
      StdErr: "wb-result-stderr",
      Control: "wb-result-control",
      Signal: "wb-result-signal",
      ExitCode: "wb-result-exitcode",
    }),
    []
  );

  return (
    <code>
      {mergedResults.map((r, index): React.ReactElement => {
        const className = typeClassNames[r.type];

        return (
          <pre key={index} className={className}>
            {r.data}
          </pre>
        );
      })}
    </code>
  );
};

export { Result };
