import { renderToString } from "react-dom/server";
import { RemixServer } from "remix";
import type { EntryContext } from "remix";

import { commitSession, getSession } from "./sessions.server";

async function getGithubAccessToken(url: URL): Promise<string | null> {
  const client_id = WANDBOX_GITHUB_CLIENT_ID;
  const client_secret = WANDBOX_GITHUB_CLIENT_SECRET;
  const code = url.searchParams.get("code");
  const body = JSON.stringify({
    client_id,
    client_secret,
    code,
  });
  const headers = {
    "Content-Type": "application/json",
    Accept: "application/json",
  };
  const resp = await fetch("https://github.com/login/oauth/access_token", {
    method: "POST",
    headers,
    body,
  });
  if (!resp.ok) {
    return null;
  }
  const json = await resp.json();
  if (!("access_token" in json)) {
    console.error(json);
    return null;
  }
  return json["access_token"] as string;
}

async function getGithubUser(accessToken: string): Promise<GithubUser | null> {
  const headers = {
    Authorization: `token ${accessToken}`,
    Accept: "application/json",
    "User-Agent": "Wandbox",
  };
  const resp = await fetch("https://api.github.com/user", { headers });
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
  return json as GithubUser;
}

export default async function handleRequest(
  request: Request,
  responseStatusCode: number,
  responseHeaders: Headers,
  remixContext: EntryContext
) {
  const hasError = remixContext.appState.error !== undefined;
  // GitHub 認証
  const url = new URL(request.url);
  if (
    !hasError &&
    request.method === "GET" &&
    url.pathname === "/login/github/callback"
  ) {
    const accessToken = await getGithubAccessToken(url);
    if (accessToken !== null) {
      const user = await getGithubUser(accessToken);
      if (user !== null) {
        const session = await getSession(request.headers.get("Cookie"));
        session.set("github_user", JSON.stringify(user));
        await commitSession(session);
      }
    }
    console.log(url);
    return Response.redirect(url.origin + "/", 301);
  }
  // ログアウト
  if (!hasError && request.method === "GET" && url.pathname === "/logout") {
    const session = await getSession(request.headers.get("Cookie"));
    session.unset("github_user");
    await commitSession(session);
    return Response.redirect(url.origin + "/", 301);
  }
  // permlink 送信リクエストにユーザー情報を設定する
  if (
    !hasError &&
    request.method === "POST" &&
    url.pathname === "/api/permlink"
  ) {
    const json = await request.json();
    const session = await getSession(request.headers.get("Cookie"));
    if (session.has("github_user")) {
      const githubUser = JSON.parse(session.get("github_user"));
      json["login"] = githubUser["login"];
      json["github_id"] = githubUser["id"];
    }
    const headers = {
      "Content-Type": "application/json",
      Accept: "application/json",
    };
    const resp = await fetch(`${WANDBOX_URL_PREFIX}/api/permlink`, {
      method: "POST",
      headers: headers,
      body: JSON.stringify(json),
    });
    return resp;
  }
  // それ以外の API リクエストは単に転送する
  if (!hasError && url.pathname.startsWith("/api")) {
    let json: any = null;
    if (request.method === "POST") {
      json = await request.json();
    }
    const headers = {
      "Content-Type": "application/json",
      Accept: "application/json",
    };
    const resp = await fetch(`${WANDBOX_URL_PREFIX}${url.pathname}`, {
      method: request.method,
      headers: headers,
      body: json === null ? undefined : JSON.stringify(json),
    });
    return resp;
  }

  let markup = renderToString(
    <RemixServer context={remixContext} url={request.url} />
  );

  responseHeaders.set("Content-Type", "text/html");

  return new Response("<!DOCTYPE html>" + markup, {
    status: responseStatusCode,
    headers: responseHeaders,
  });
}
