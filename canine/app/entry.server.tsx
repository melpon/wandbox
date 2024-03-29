import { renderToString } from "react-dom/server";
import { RemixServer } from "remix";
import type { EntryContext } from "remix";

import { commitSession, getSession } from "./sessions.server";
import { AnyJson } from "./hooks/fetch";

class WandboxError extends Error {
  constructor(public statusCode: number, public errorMessage: string) {
    super(`${statusCode} ${errorMessage}`);
    this.name = new.target.name;
  }
}

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

// Cloudflare Workers からサブリクエストを投げる場合、
// CF-Connecting-IP が Cloudflare Workers の IP になってしまうことがある。
// なのでこの値を X-Real-IP に設定して、サブリクエスト側ではそれを利用してもらう
function withClientIP(headers: HeadersInit, request: Request): HeadersInit {
  const ip = request.headers.get("cf-connecting-ip");
  if (ip === null) {
    return headers;
  }
  return {
    ...headers,
    "X-Forwarded-For": ip,
  };
}

// CORS ヘッダーの設定
function setCors(headers: Headers, request: Request): void {
  const allowHeaders = request.headers.get("Access-Control-Request-Headers");
  if (allowHeaders !== null) {
    headers.set("Access-Control-Allow-Headers", allowHeaders);
  }
  headers.set("Access-Control-Allow-Origin", "*");
}

export async function fetchPermlinkData(
  permlinkId: string,
  request: Request
): Promise<AnyJson> {
  const permlinkKey = `permlink-${permlinkId}`;
  const cache = await KV_CACHE.get(permlinkKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
    },
    request
  );
  const resp = await fetch(`${WANDBOX_URL_PREFIX}/api/permlink/${permlinkId}`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json();

  // 30日間キャッシュする
  await KV_CACHE.put(permlinkKey, JSON.stringify(body), {
    expirationTtl: 30 * 24 * 60 * 60,
  });

  return body;
}

export async function fetchListData(request: Request): Promise<AnyJson> {
  const listKey = "list";
  const cache = await KV_CACHE.get(listKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
    },
    request
  );
  const resp = await fetch(`${WANDBOX_URL_PREFIX}/api/list.json`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json();

  // 10分間キャッシュする
  await KV_CACHE.put(listKey, JSON.stringify(body), {
    expirationTtl: 10 * 60,
  });

  return body;
}

export async function fetchSponsorsData(request: Request): Promise<AnyJson> {
  const sponsorsKey = "sponsors";
  const cache = await KV_CACHE.get(sponsorsKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
    },
    request
  );
  const resp = await fetch(`${WANDBOX_URL_PREFIX}/api/sponsors.json`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json();

  // 10分間キャッシュする
  await KV_CACHE.put(sponsorsKey, JSON.stringify(body), {
    expirationTtl: 10 * 60,
  });

  return body;
}

function redirectWithCookie(url: string, cookie: string): Response {
  return new Response(null, {
    status: 302,
    headers: {
      Location: url,
      "Set-Cookie": cookie,
    },
  });
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
        return redirectWithCookie(
          url.origin + "/",
          await commitSession(session, {
            maxAge: 30 * 24 * 60 * 60,
            sameSite: "lax",
          })
        );
      }
    }
    return Response.redirect(url.origin + "/", 302);
  }
  // ログアウト
  if (!hasError && request.method === "GET" && url.pathname === "/logout") {
    const session = await getSession(request.headers.get("Cookie"));
    session.unset("github_user");

    return redirectWithCookie(
      url.origin + "/",
      await commitSession(session, {
        maxAge: 30 * 24 * 60 * 60,
        sameSite: "lax",
      })
    );
  }
  try {
    // OPTIONS リクエストは通す
    if (
      !hasError &&
      request.method === "OPTIONS" &&
      url.pathname.startsWith("/api")
    ) {
      setCors(responseHeaders, request);
      return new Response("", { headers: responseHeaders });
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
        json["github_user"] = githubUser["login"];
        json["github_id"] = githubUser["id"];
      }
      const headers = withClientIP(
        {
          "Content-Type": "application/json",
          Accept: "application/json",
        },
        request
      );
      const resp = await fetch(`${WANDBOX_URL_PREFIX}/api/permlink`, {
        method: "POST",
        headers: headers,
        body: JSON.stringify(json),
      });
      return resp;
    }
    // permlink の取得はキャッシュする
    if (
      !hasError &&
      request.method === "GET" &&
      url.pathname.startsWith("/api/permlink")
    ) {
      const permlinkId = url.pathname
        .replace("/api/permlink/", "")
        .replace("/", "");
      const json = await fetchPermlinkData(permlinkId, request);
      responseHeaders.set("Content-Type", "application/json");
      setCors(responseHeaders, request);
      return new Response(JSON.stringify(json), { headers: responseHeaders });
    }
    // sponsors の取得もキャッシュする
    if (
      !hasError &&
      request.method === "GET" &&
      url.pathname.startsWith("/api/sponsors.json")
    ) {
      const json = await fetchSponsorsData(request);
      responseHeaders.set("Content-Type", "application/json");
      setCors(responseHeaders, request);
      return new Response(JSON.stringify(json), { headers: responseHeaders });
    }
    // list の取得もキャッシュする
    if (
      !hasError &&
      request.method === "GET" &&
      url.pathname.startsWith("/api/list.json")
    ) {
      const json = await fetchListData(request);
      responseHeaders.set("Content-Type", "application/json");
      setCors(responseHeaders, request);
      return new Response(JSON.stringify(json), { headers: responseHeaders });
    }
    // それ以外の API リクエストは単に転送する
    if (!hasError && url.pathname.startsWith("/api")) {
      let json: any = null;
      if (request.method === "POST") {
        json = await request.json();
      }
      const headers = withClientIP(
        {
          "Content-Type": "application/json",
          Accept: "application/json",
        },
        request
      );
      const resp = await fetch(`${WANDBOX_URL_PREFIX}${url.pathname}`, {
        method: request.method,
        headers: headers,
        body: json === null ? undefined : JSON.stringify(json),
      });
      responseHeaders.set(
        "Content-Type",
        resp.headers.get("content-type") ?? "application/json"
      );
      setCors(responseHeaders, request);
      return new Response(await resp.text(), {
        headers: responseHeaders,
        status: resp.status,
        statusText: resp.statusText,
      });
    }
  } catch (e) {
    if (e instanceof WandboxError) {
      return new Response(e.errorMessage, { status: e.statusCode });
    } else {
      throw e;
    }
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
