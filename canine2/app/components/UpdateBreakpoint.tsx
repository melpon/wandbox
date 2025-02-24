import React, { useCallback, useEffect, useLayoutEffect } from "react";
import { wandboxSlice } from "~/features/slice";
import type { Breakpoint } from "~/features/slice";
import useIsomorphicLayoutEffect from "~/hooks/useIsomorphicLayoutEffect";
import { useAppDispatch } from "~/store";

// Breakpoint の変更を検出して反映するコンポーネント
// レンダリングは行わない
const UpdateBreakpoint: React.FC = () => {
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;

  // 現在の breakpoint を設定する
  const updateBreakpoint = useCallback(() => {
    const breakpoints = ["xxl", "xl", "lg", "md", "sm", "xs"].map(
      (x) =>
        [
          x,
          parseInt(
            getComputedStyle(document.body).getPropertyValue(
              `--wb-breakpoint-${x}`
            ),
            10
          ),
        ] as [Breakpoint, number]
    );

    const width = document.documentElement.clientWidth;
    for (const [bp, value] of breakpoints) {
      if (width >= value) {
        dispatch(actions.setBreakpoint(bp));
        break;
      }
    }
  }, []);

  useIsomorphicLayoutEffect(() => {
    updateBreakpoint();
  }, []);

  useEffect(() => {
    window.addEventListener("resize", updateBreakpoint);
    window.addEventListener("orientationchange", updateBreakpoint);
    return () => {
      window.removeEventListener("resize", updateBreakpoint);
      window.removeEventListener("orientationchange", updateBreakpoint);
    };
  }, [updateBreakpoint]);

  return null;
};

export { UpdateBreakpoint };
