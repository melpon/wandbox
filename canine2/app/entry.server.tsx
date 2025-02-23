/**
 * By default, Remix will handle generating the HTTP Response for you.
 * You are free to delete this file if you'd like to, but if you ever want it revealed again, you can run `npx remix reveal` ✨
 * For more information, see https://remix.run/file-conventions/entry.server
 */

import type { AppLoadContext, EntryContext } from "@remix-run/cloudflare";
import { RemixServer } from "@remix-run/react";
import { isbot } from "isbot";
import { renderToReadableStream } from "react-dom/server";
import { AnyJson } from "./hooks/fetch";
import { getSessionStorage } from "~/sessions.server";
import type { GithubAccessToken, GithubUser } from "~/types";

class WandboxError extends Error {
  constructor(public statusCode: number, public errorMessage: string) {
    super(`${statusCode} ${errorMessage}`);
    this.name = new.target.name;
  }
}

async function getGithubAccessToken(env: Env, url: URL): Promise<string | null> {
  const client_id = env.WANDBOX_GITHUB_CLIENT_ID;
  const client_secret = env.WANDBOX_GITHUB_CLIENT_SECRET;
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
  const json = await resp.json<GithubAccessToken | {access_token: undefined}>();
  if (json.access_token === undefined) {
    console.error(json);
    return null;
  }
  return json.access_token;
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
  const json = await resp.json<GithubUser | {id: undefined}>();
  if (json.id === undefined) {
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
  env: Env,
  permlinkId: string,
  request: Request
): Promise<AnyJson> {
  const permlinkKey = `permlink-${permlinkId}`;
  const cache = await env.KV_CACHE.get(permlinkKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
    },
    request
  );
  const resp = await fetch(`${env.WANDBOX_URL_PREFIX}/api/permlink/${permlinkId}`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json<AnyJson>();

  // 30日間キャッシュする
  await env.KV_CACHE.put(permlinkKey, JSON.stringify(body), {
    expirationTtl: 30 * 24 * 60 * 60,
  });

  return body;
}

export async function fetchListData(env: Env, request: Request): Promise<AnyJson> {
  const listKey = "list";
  const cache = await env.KV_CACHE.get(listKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
      "Accept-Encoding": "identity", 
    },
    request
  );
  const resp = await fetch(`${env.WANDBOX_URL_PREFIX}/api/list.json`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json<AnyJson>();

  // 10分間キャッシュする
  await env.KV_CACHE.put(listKey, JSON.stringify(body), {
    expirationTtl: 10 * 60,
  });

  return body;
}

export async function fetchSponsorsData(env: Env, request: Request): Promise<AnyJson> {
  const sponsorsKey = "sponsors";
  const cache = await env.KV_CACHE.get(sponsorsKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const headers = withClientIP(
    {
      "content-type": "application/json",
      "Accept-Encoding": "identity", 
    },
    request
  );
  const resp = await fetch(`${env.WANDBOX_URL_PREFIX}/api/sponsors.json`, {
    headers: headers,
  });
  if (resp.status !== 200) {
    throw new WandboxError(resp.status, await resp.text());
  }
  const body = await resp.json<AnyJson>();

  // 10分間キャッシュする
  await env.KV_CACHE.put(sponsorsKey, JSON.stringify(body), {
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

const ABORT_DELAY = 5000;

export default async function handleRequest(
  request: Request,
  responseStatusCode: number,
  responseHeaders: Headers,
  remixContext: EntryContext,
  // This is ignored so we can keep it in the template for visibility.  Feel
  // free to delete this parameter in your app if you're not using it!
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  loadContext: AppLoadContext
) {
  // const controller = new AbortController();
  // const timeoutId = setTimeout(() => controller.abort(), ABORT_DELAY);

  // const body = await renderToReadableStream(
  //   <RemixServer
  //     context={remixContext}
  //     url={request.url}
  //     abortDelay={ABORT_DELAY}
  //   />,
  //   {
  //     signal: controller.signal,
  //     onError(error: unknown) {
  //       if (!controller.signal.aborted) {
  //         // Log streaming rendering errors from inside the shell
  //         console.error(error);
  //       }
  //       responseStatusCode = 500;
  //     },
  //   }
  // );

  // body.allReady.then(() => clearTimeout(timeoutId));

  // if (isbot(request.headers.get("user-agent") || "")) {
  //   await body.allReady;
  // }

  // responseHeaders.set("Content-Type", "text/html");
  // return new Response(body, {
  //   headers: responseHeaders,
  //   status: responseStatusCode,
  // });
  const hasError = false;// remixContext.appState.error !== undefined;
  const url = new URL(request.url);
  const env = loadContext.cloudflare.env;
  const ss = getSessionStorage(env);

  // GitHub 認証
  if (
    !hasError &&
    request.method === "GET" &&
    url.pathname === "/login/github/callback"
  ) {
    const accessToken = await getGithubAccessToken(env, url);
    if (accessToken !== null) {
      const user = await getGithubUser(accessToken);
      if (user !== null) {
        const session = await ss.getSession(request.headers.get("Cookie"));
        session.set("github_user", JSON.stringify(user));
        return redirectWithCookie(
          url.origin + "/",
          await ss.commitSession(session, {
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
    const session = await ss.getSession(request.headers.get("Cookie"));
    session.unset("github_user");

    return redirectWithCookie(
      url.origin + "/",
      await ss.commitSession(session, {
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
      const json = await request.json<any>();
      const session = await ss.getSession(request.headers.get("Cookie"));
      if (session.has("github_user")) {
        const githubUser = JSON.parse(session.get("github_user"));
        json["github_user"] = githubUser["login"];
        json["github_id"] = githubUser["id"];
      }
      const headers = withClientIP(
        {
          "Content-Type": "application/json",
          "Accept-Encoding": "application/json",
        },
        request
      );
      const resp = await fetch(`${env.WANDBOX_URL_PREFIX}/api/permlink`, {
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
      const json = await fetchPermlinkData(env, permlinkId, request);
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
      const json = await fetchSponsorsData(env, request);
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
      const json = await fetchListData(env, request);
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
      const resp = await fetch(`${env.WANDBOX_URL_PREFIX}${url.pathname}`, {
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

  const body = await renderToReadableStream(
    <RemixServer context={remixContext} url={request.url} />
  );

  if (isbot(request.headers.get("user-agent") || "")) {
    await body.allReady;
  }

  responseHeaders.set("Content-Type", "text/html");

  return new Response(body, {
    status: responseStatusCode,
    headers: responseHeaders,
  });
}
