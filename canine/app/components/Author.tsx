import React, { useEffect, useState } from "react";
import { PermlinkData } from "~/hooks/permlink";
import { formatDistanceToNow } from "date-fns";
import { AppState, useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";
import { useSelector } from "react-redux";

export interface AuthorDataProps {
  permlinkData: PermlinkData;
}
export interface AuthorProps {
  permlinkData: PermlinkData;
  author: GithubUser | null;
}

async function getGithubUser(username: string): Promise<GithubUser | null> {
  const headers = {
    Accept: "application/json",
  };
  const resp = await fetch(`https://api.github.com/users/${username}`, {
    headers,
  });
  if (!resp.ok) {
    console.error(resp);
    console.error(await resp.text());
    return null;
  }
  const json = await resp.json();
  if (!("id" in json)) {
    console.error(json);
    return null;
  }
  // localStorage にも保存することがあるため、余分な情報を落とす
  return {
    id: json.id,
    login: json.login,
    avatar_url: json.avatar_url,
    html_url: json.html_url,
  };
}

// Author はスマホ用とPC用で別の場所に置くことになったので、
// 副作用は AuthorData に閉じ込めて Author では表示だけを行う。
const AuthorData: React.FC<AuthorDataProps> = ({ permlinkData }) => {
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
  const author = useSelector(({ wandbox: { author } }: AppState) => author);

  useEffect(() => {
    const username = permlinkData.parameter.githubUser;
    if (username.length === 0) {
      // Permlink の履歴を追加
      // username がある場合、GitHub から情報の取得が終わってから追加する
      dispatch(
        actions.pushPermlink({
          permlinkId: permlinkData.permlinkId,
          githubUser: null,
          currentLanguage: permlinkData.parameter.compilerInfo.language,
          displayName: permlinkData.parameter.compilerInfo.displayName,
          version: permlinkData.parameter.compilerInfo.version,
          title: permlinkData.parameter.title,
          permlinkCreatedAt: permlinkData.parameter.createdAt,
        })
      );
      dispatch(actions.setAuthor(null));
      return;
    }
    dispatch(actions.setAuthor(null));
    getGithubUser(username).then((author) =>
      dispatch(actions.setAuthor(author))
    );
  }, [permlinkData.parameter.githubUser]);

  const distanceTime = formatDistanceToNow(
    new Date(permlinkData.parameter.createdAt * 1000),
    { addSuffix: true }
  );

  useEffect(() => {
    const username = permlinkData.parameter.githubUser;
    if (username.length === 0 || author === null) {
      return;
    }
    // Permlink の履歴を追加
    dispatch(
      actions.pushPermlink({
        permlinkId: permlinkData.permlinkId,
        githubUser: author,
        currentLanguage: permlinkData.parameter.compilerInfo.language,
        displayName: permlinkData.parameter.compilerInfo.displayName,
        version: permlinkData.parameter.compilerInfo.version,
        title: permlinkData.parameter.title,
        permlinkCreatedAt: permlinkData.parameter.createdAt,
      })
    );
  }, [author]);

  return null;
};

const Author: React.FC<AuthorProps> = ({ permlinkData, author }) => {
  const distanceTime = formatDistanceToNow(
    new Date(permlinkData.parameter.createdAt * 1000),
    { addSuffix: true }
  );

  const loading =
    permlinkData.parameter.githubUser.length !== 0 && author === null;

  if (loading) {
    return null;
  }

  return (
    <div className="wb-author d-flex flex-column align-self-start">
      <p className="wb-label px-4px py-2px">Author</p>
      <div className="wb-card d-flex px-4px py-4px gap-4px">
        <div className="d-flex flex-column flex-grow-1">
          {author === null ? (
            <p>anonymous</p>
          ) : (
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={author.html_url}
            >{`@${author.login}`}</a>
          )}
          <p className="wb-time">{distanceTime}</p>
        </div>
        <div style={{ width: 40, height: 40 }}>
          {author !== null && (
            <a target="_blank" rel="noopener noreferrer" href={author.html_url}>
              <img
                className="w-100 h-100"
                style={{ borderRadius: "50%" }}
                src={author.avatar_url}
              />
            </a>
          )}
        </div>
      </div>
    </div>
  );
};

export { AuthorData, Author };
