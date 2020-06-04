import React from "react";

export type AnyJson = string | number | boolean | null | JsonMap | AnyJson[];
export type JsonMap = { [property: string]: AnyJson };

type Resolver<T> = (resp: AnyJson | string | Blob | FormData) => T;

/**
 * ネットワークからデータを取得する
 *
 * `fetch()` 関数をフック化したものとなる。
 *
 * 以下のように利用する。
 *
 * ```
 * const MyComponent = () => {
 *   const [response, fetchId, doFetch] = useFetch<any>("GET", url, defaultOpts, r => r, onError)
 *   useEffect(() => {
 *     doFetch(null, opts);
 *   }, [])
 *   if (response === null) {
 *     return null;
 *   }
 * }
 * ```
 *
 * doFetch を呼ぶまではリクエストが発生しないことに注意すること。
 *
 * fetchId は、レスポンスが完了した時に変化する値である。
 * そのため以下のように書くことで「レスポンスが返ってきた時に特定の処理を実行する」ということが可能になる。
 *
 * ```
 * // GET リクエストと POST リクエストを作る
 * const [data, fetchId, doFetch] = useFetch<any>(url, {method: "GET"}, r => r, onError)
 * const [data2, fetchId2, doFetch2] = useFetch<any>(url2, {method: "POST"}, r => r, onError)
 *
 * // 初回のレンダリング時、あるいは POST リクエストが完了した時に GET リクエストを呼び出す
 * useEffect(() => {
 *   doFetch(null, {});
 * }, [fetchId2])
 *
 * // クリックするたびに doFetch2 を呼び出す。
 * const onClick = useCallback(() => {
 *   doFetch2(null, {});
 * }, [doFetch2]);
 *
 * return (
 *     <Hoge onClick={onClick} />
 * )
 * ```
 *
 * @param mode - レスポンスボディの解決方法
 * @param defaultUrl - リクエスト先 URL のデフォルト値。デフォルト値が不要な場合は null を指定して構わない。
 * @param defaultOpts - fetch に渡す各種パラメータのデフォルト値。ただし abortController はこの関数の中で上書きされるので利用してはいけない。
 * @param resolver - レスポンスを戻り値の型 T に変換するための関数
 * @param onError - エラーが起きた時に呼ばれるコールバック
 *
 * @returns ３要素のタプル。
 *   最初の要素には、レスポンスボディか、取得できてなければnullが入っている。
 *   ２番めの要素には、レスポンスが完了するたびに変化する値が入っている。
 *   ３番目の要素には、実際にリクエストを呼び出すための関数が入っている。
 */
export function useFetch<T>(
  mode: "json" | "text" | "blob" | "formData",
  defaultUrl: string | null,
  defaultOpts: RequestInit,
  resolver: Resolver<T>,
  onError: (error: string) => void
): [T | null, number, (url: string | null, opts: RequestInit) => void] {
  const [response, setResponse] = React.useState<T | null>(null);
  const [counter, setCounter] = React.useState<number>(0);
  const [fetchCounter, setFetchCounter] = React.useState<number>(0);
  const [url, setUrl] = React.useState<string | null>(null);
  const [opts, setOpts] = React.useState<RequestInit>({});

  const didMountRef = React.useRef(false);

  React.useEffect((): (() => void) | undefined => {
    // 初回は実行しない
    if (!didMountRef.current) {
      didMountRef.current = true;
      return;
    }

    const abortController = new AbortController();
    (async (): Promise<void> => {
      if (url === null) {
        onError("URL が指定されていません");
        return;
      }

      try {
        const payload = await fetch(url, {
          ...opts,
          signal: abortController.signal,
        });

        if (payload.status !== 200) {
          onError(
            `${url} リクエストエラー: ステータスコード=${payload.status}`
          );
          return;
        }
        if (mode === "json") {
          setResponse(resolver((await payload.json()) as AnyJson));
        } else if (mode === "text") {
          setResponse(resolver(await payload.text()));
        } else if (mode === "blob") {
          setResponse(resolver(await payload.blob()));
        } else if (mode === "formData") {
          setResponse(resolver(await payload.formData()));
        }
        setFetchCounter((counter): number => counter + 1);
      } catch (error) {
        onError(`${url} リクエストエラー: エラー=${String(error)}`);
      }
    })();
    const cleanup = (): void => {
      abortController.abort();
    };
    return cleanup;
  }, [counter]);

  const doFetch = React.useCallback(
    (url, opts): void => {
      setUrl(url || defaultUrl);
      setOpts(Object.assign(defaultOpts, opts));
      setCounter((counter): number => counter + 1);
    },
    [defaultUrl, defaultOpts]
  );
  return [response, fetchCounter, doFetch];
}

/**
 * `useFetch` の `mode` を `"json"` に固定したバージョン
 */
export function useFetchJSON<T>(
  url: string | null,
  opts: RequestInit,
  resolver: (resp: AnyJson) => T,
  onError: (error: string) => void
): [T | null, number, (url: string | null, opts: RequestInit) => void] {
  return useFetch<T>("json", url, opts, resolver as Resolver<T>, onError);
}

/**
 * `useFetch` の `mode` を `"text"` に固定したバージョン
 */
export function useFetchText<T>(
  url: string | null,
  opts: RequestInit,
  resolver: (resp: string) => T,
  onError: (error: string) => void
): [T | null, number, (url: string | null, opts: RequestInit) => void] {
  return useFetch<T>("text", url, opts, resolver as Resolver<T>, onError);
}

/**
 * `useFetch` の `mode` を `"blob"` に固定したバージョン
 */
export function useFetchBlob<T>(
  url: string | null,
  opts: RequestInit,
  resolver: (resp: Blob) => T,
  onError: (error: string) => void
): [T | null, number, (url: string | null, opts: RequestInit) => void] {
  return useFetch("blob", url, opts, resolver as Resolver<T>, onError);
}

/**
 * `useFetch` の `mode` を `"formData"` に固定したバージョン
 */
export function useFetchFormData<T>(
  url: string | null,
  opts: RequestInit,
  resolver: (resp: FormData) => T,
  onError: (error: string) => void
): [T | null, number, (url: string | null, opts: RequestInit) => void] {
  return useFetch("blob", url, opts, resolver as Resolver<T>, onError);
}
