import React, { useEffect, useState } from "react";
import { PermlinkData } from "~/hooks/permlink";
import { formatDistanceToNow } from "date-fns";
import { useAppDispatch } from "~/store";
import { wandboxSlice } from "~/features/slice";

export interface AuthorProps {
  permlinkData: PermlinkData;
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

const Author: React.FC<AuthorProps> = ({ permlinkData }) => {
  const [user, setUser] = useState<GithubUser | null>(null);
  const dispatch = useAppDispatch();
  const actions = wandboxSlice.actions;
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
          currentCompilerName: permlinkData.parameter.compilerInfo.displayName,
          title: permlinkData.parameter.title,
          permlinkCreatedAt: permlinkData.parameter.createdAt,
        })
      );
      return;
    }
    if (user !== null) {
      return;
    }
    getGithubUser(username).then(setUser);
  }, [user]);

  const distanceTime = formatDistanceToNow(
    new Date(permlinkData.parameter.createdAt * 1000),
    { addSuffix: true }
  );

  useEffect(() => {
    const username = permlinkData.parameter.githubUser;
    if (username.length === 0 || user === null) {
      return;
    }
    // Permlink の履歴を追加
    dispatch(
      actions.pushPermlink({
        permlinkId: permlinkData.permlinkId,
        githubUser: user,
        currentLanguage: permlinkData.parameter.compilerInfo.language,
        currentCompilerName: permlinkData.parameter.compilerInfo.displayName,
        title: permlinkData.parameter.title,
        permlinkCreatedAt: permlinkData.parameter.createdAt,
      })
    );
  }, [user]);

  return (
    <div className="wb-author d-flex flex-column align-self-start">
      <p className="wb-label px-4px py-2px">Author</p>
      <div className="wb-card d-flex px-4px py-4px gap-4px">
        <div className="d-flex flex-column flex-grow-1">
          {user === null ? (
            <p>anonymous</p>
          ) : (
            <a
              target="_blank"
              rel="noopener noreferrer"
              href={user.html_url}
            >{`@${user.login}`}</a>
          )}
          <p className="wb-time">{distanceTime}</p>
        </div>
        <div style={{ width: 40, height: 40 }}>
          {user !== null && (
            <a target="_blank" rel="noopener noreferrer" href={user.html_url}>
              <img
                className="w-100 h-100"
                style={{ borderRadius: "50%" }}
                src={user.avatar_url}
              />
            </a>
          )}
        </div>
      </div>
    </div>
  );
};

export { Author };
