/**
 * 書き換え可能な Context を作るための仕組み
 *
 * つまり Context をグローバル変数として扱えるようにする。
 *
 * 使い方:
 *
 * 1. createContext を使って Context を作る。
 *
 * ```
 * import { createContext } from "./WithContext";
 *
 * export interface PlayerContextState {
 *   gameId: string;
 *   gameEnv: string;
 *   playerId: string;
 * }
 *
 * export const playerInitialState: PlayerContextState = {
 *   gameId: "",
 *   gameEnv: "",
 *   playerId: ""
 * };
 *
 * export const PlayerContext = createContext<PlayerContextState>(
 *   playerInitialState
 * );
 * ```
 *
 * 2. WithContext を使って Context Provider を記述する。
 *
 * ```
 * import { WithContext } from "./WithContext";
 * import {
 *   PlayerContext,
 *   PlayerContextState,
 *   playerInitialState
 * } from "./PlayerContext";
 *
 * export const MyApp: React.FC<MyAppProps> = (props): React.ReactElement => {
 *   return (
 *     <WithContext<PlayerContextState>
 *       initialState={playerInitialState}
 *       provider={PlayerContext.Provider}
 *     >
 *       {...}
 *     </WithContext>
 *   );
 * };
 * ```
 *
 * 3. 必要な場所で useContext を利用して、state, setState で読み書きする
 *
 * ```
 * import { PlayerContext } from "./PlayerContext";
 *
 * export const MyComponent: React.FC<MyComponentProps> = (props): React.ReactElement => {
 *   const playerContext = React.useContext(PlayerContext)
 *   // playerContext.state で PlayerContextState 型の値が取得できる
 *   // playerContext.setState で PlayerContextState 型の値を設定できる
 *
 *   // Context データの読み込み
 *   const { gameId, gameEnv, playerId } = playerContext.state;
 *   // Context データの書き込み
 *   playerContext.setState({
 *     gameId: "foo",
 *     gameEnv: "bar",
 *     playerId: "",
 *   })
 *   // 関数形式でも設定可能
 *   playerContext.setState(state => ({
 *     gameId: "newGameId",
 *     ...state
 *   }))
 * }
 * ```
 */
import React from "react";

export interface ContextType<T> {
  state: T;
  setState: React.Dispatch<React.SetStateAction<T>>;
}

export function createContext<T>(
  initialState: T
): React.Context<ContextType<T>> {
  return React.createContext<ContextType<T>>({
    state: initialState,
    setState: (): void => {}
  });
}

export interface WithContextProps<T> {
  initialState: T;
  provider: React.Provider<ContextType<T>>;
}

export function WithContext<T>(
  props: React.PropsWithChildren<WithContextProps<T>>
): React.ReactElement {
  const { children, initialState } = props;
  const [state, setState] = React.useState<T>(initialState);
  const context: ContextType<T> = {
    state,
    setState
  };
  return <props.provider value={context}>{children}</props.provider>;
}
