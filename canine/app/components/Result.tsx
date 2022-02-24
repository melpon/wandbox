import React, { useEffect, useLayoutEffect, useMemo, useRef } from "react";
import { Trans, useTranslation } from "react-i18next";
import { useSelector } from "react-redux";

import type { ResultData } from "~/features/slice";
import type { PermlinkData } from "~/hooks/permlink";
import type { AppState } from "~/store";

interface ResultProps {
  permlinkData: PermlinkData | null;
}

const Result: React.FC<ResultProps> = (props): React.ReactElement | null => {
  const { permlinkData } = props;
  const {
    running,
    results: rs,
    fixedResultHeight,
  } = useSelector(
    ({
      wandbox: {
        running,
        results,
        editorSettings: { fixedResultHeight },
      },
    }: AppState) => ({
      running,
      results,
      fixedResultHeight,
    })
  );
  const results = permlinkData === null ? rs : permlinkData.results;
  const ref = useRef<HTMLDivElement>(null);
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
      const v: ResultData = {
        type: r.type,
        data: preview.data + r.data,
      };
      mergedResults[mergedResults.length - 1] = v;
      preview = v;
    } else {
      mergedResults.push(r);
      preview = r;
    }
  }
  const typeClassNames = useMemo(
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

  // スクロールを一番下に持ってくる
  useLayoutEffect(() => {
    if (ref.current === null || !running) {
      return;
    }
    const elem = fixedResultHeight
      ? document.querySelector("#wb-result-console")
      : document.querySelector("#wb-main-content");
    if (elem === null) {
      return;
    }
    elem.scrollTop = ref.current.scrollHeight;
  }, [results]);

  if (results.length === 0) {
    return null;
  }

  return (
    <div className="wb-result d-flex flex-column">
      <code
        ref={ref}
        id="wb-result-console"
        className={`wb-console ${
          fixedResultHeight ? "wb-result-fixedheight" : ""
        } p-16px`}
      >
        {mergedResults.map((r, index): React.ReactElement | null => {
          if (
            r.type === "Control" ||
            r.type === "Signal" ||
            r.type === "ExitCode"
          ) {
            return null;
          }
          const className = typeClassNames[r.type];

          return (
            <pre key={index} className={className}>
              {r.data}
            </pre>
          );
        })}
      </code>
      <div className="wb-info">
        {mergedResults.map((r, index): React.ReactElement | null => {
          if (r.type === "Control") {
            return null;
          }

          if (r.type !== "Signal" && r.type !== "ExitCode") {
            return null;
          }

          return (
            <div key={index} className="d-flex align-items-center gap-8px">
              {r.type === "Signal" ? (
                <Trans
                  i18nKey="result.signal"
                  values={{ signal: r.data }}
                  components={{
                    d1: <div className="wb-name" />,
                    d2: <div className="wb-value" />,
                  }}
                />
              ) : (
                <Trans
                  i18nKey="result.exitcode"
                  values={{ exitCode: r.data }}
                  components={{
                    d1: <div className="wb-name" />,
                    d2: <div className="wb-value" />,
                  }}
                />
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export { Result };
