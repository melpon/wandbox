/**
 * エラーを一箇所で纏めて表示するためのコンポーネントとフックを提供する。
 *
 * 使い方:
 *
 * エラーのコンテキストを保持するため、`<Error>` コンポーネントをコンポーネントツリーの上位に設置する。
 *
 * ```tsx
 * const App = () => {
 *   return (
 *     <Error>
 *       <MyErrorComponent />
 *       <MyComponent />
 *     </Error>
 *   );
 * }
 * ```
 *
 * `<Error>` 以下で `useError()` を呼び出して、エラーが発生した時の処理を記述する。
 *
 * ```tsx
 * const MyErrorComponent = () => {
 *   [error, setError] = useError();
 *   const onClose = useCallback(() => setError(null));
 *   // エラーが起きていたらポップアップを表示して、ポップアップがクリックされたらエラーを閉じるようにする
 *   return (
 *     <MyPopupComponent
 *       open={error != null}
 *       onClick={onClose}
 *     >
 *       {error}
 *     </MyPopupComponent>
 *   );
 * }
 * ```
 *
 * あとは `useError()` を使って、エラーが発生した時にエラーを設定する。
 *
 * ```tsx
 * const MyComponent = () => {
 *   [, setError] = useError();
 *   useEffect(() => {
 *     // 非同期にデータを取得して、エラーが起きた時には setError を呼び出す。
 *     (async () => {
 *       try {
 *         const payload = await fetch("http://example.com/");
 *         // ...
 *       } catch (error) {
 *         setError(String(error))
 *       }
 *     })();
 *   })
 * }
 * ```
 */
import React from "react";

interface ErrorState {
  valid: boolean;
  value: string;
}

interface ErrorContextType {
  state: ErrorState;
  setState: React.Dispatch<React.SetStateAction<ErrorState>>;
}

const ErrorContext = React.createContext<ErrorContextType>({
  state: {
    valid: false,
    value: ""
  },
  setState: (): void => {}
});

export const Error: React.FC<{}> = (props): React.ReactElement => {
  const [state, setState] = React.useState<ErrorState>({
    valid: false,
    value: ""
  });
  const context = {
    state: state,
    setState: setState
  };

  return (
    <ErrorContext.Provider value={context}>
      {props.children}
    </ErrorContext.Provider>
  );
};

export function useError(): [string | null, (error: string | null) => void] {
  const context = React.useContext<ErrorContextType>(ErrorContext);
  const error = context.state.valid ? context.state.value : null;
  const setError = React.useCallback(
    (error: string | null): void => {
      context.setState({
        valid: error !== null,
        value: error === null ? "" : error
      });
    },
    [context]
  );
  return [error, setError];
}
