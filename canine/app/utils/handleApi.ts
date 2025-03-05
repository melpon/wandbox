import { Hpplib, MergedHppInfo } from "~/features/slice";
import { CompilerInfo, CompilerList, resolveCompilerInfo } from "~/hooks/compilerList";
import { AnyJson, JsonMap } from "~/hooks/fetch";
import { PermlinkData, resolvePermlinkData } from "~/hooks/permlink";
import { getSessionStorage } from "~/sessions.server";
import { GithubAccessToken, GithubUser } from "~/types";

export class WandboxError extends Error {
  constructor(public statusCode: number, public errorMessage: string) {
    super(`${statusCode} ${errorMessage}`);
    this.name = new.target.name;
  }
}

export async function getGithubAccessToken(env: Env, url: URL): Promise<string | null> {
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
  const json = await resp.json<GithubAccessToken | { access_token: undefined }>();
  if (json.access_token === undefined) {
    console.error(json);
    return null;
  }
  return json.access_token;
}

export async function getGithubUser(accessToken: string): Promise<GithubUser | null> {
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
  const json = await resp.json<GithubUser | { id: undefined }>();
  if (json.id === undefined) {
    console.error(json);
    return null;
  }
  return json as GithubUser;
}

// Cloudflare Workers からサブリクエストを投げる場合、
// CF-Connecting-IP が Cloudflare Workers の IP になってしまうことがある。
// なのでこの値を X-Real-IP に設定して、サブリクエスト側ではそれを利用してもらう
export function withClientIP(headers: HeadersInit, request: Request): HeadersInit {
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
export function withCors(headers: HeadersInit, request: Request): HeadersInit {
  const allowHeaders = request.headers.get("Access-Control-Request-Headers");
  if (allowHeaders !== null) {
    headers = { ...headers, "Access-Control-Allow-Headers": allowHeaders };
  }
  headers = { ...headers, "Access-Control-Allow-Origin": "*" };
  return headers;
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

export async function fetchHpplibData(env: Env, request: Request): Promise<AnyJson> {
  const hpplibKey = "hpplib";
  const cache = await env.KV_CACHE.get(hpplibKey, "json");
  if (cache !== null) {
    return cache as AnyJson;
  }

  const result: JsonMap = {};
  const headers = withClientIP(
    {
      "content-type": "application/json",
      "Accept-Encoding": "identity",
    },
    request
  );
  // Wandbox 本体側の hpplib.json を取得する
  {
    const resp = await fetch(`${env.WANDBOX_URL_PREFIX}/api/hpplib.json`, {
      headers: headers,
    });
    if (resp.status !== 200) {
      throw new WandboxError(resp.status, await resp.text());
    }
    const body = await resp.json<AnyJson>();
    result["wandbox"] = body;
  }
  // ローカル側の hpplib.json を取得する
  {
    const resp = await fetch(`/static/wasm/hpplib.json`, {
      headers: headers,
    });
    if (resp.status !== 200) {
      throw new WandboxError(resp.status, await resp.text());
    }
    const body = await resp.json<AnyJson>();
    result["clangd"] = body;
  }

  // 10分間キャッシュする
  await env.KV_CACHE.put(hpplibKey, JSON.stringify(result), {
    expirationTtl: 10 * 60,
  });

  return result;
}

export function mergeHpplib(hpplib: Hpplib): MergedHppInfo[] {
  const infos: MergedHppInfo[] = [];

  const names = new Set([...hpplib.wandbox, ...hpplib.clangd].map((info) => info.name));
  for (const name of names) {
    const w = hpplib.wandbox.find((info) => info.name === name);
    const c = hpplib.clangd.find((info) => info.name === name);
    if (w === undefined && c === undefined) {
      // これは無いはずだけど一応処理
      continue;
    } else if (w !== undefined && c === undefined) {
      infos.push({
        name: w.name,
        repository: w.repository,
        description: w.description,
        headerDescription: w.headerDescription,
        wandbox: {
          tagName: w.tagName,
          publishedAt: w.publishedAt,
        },
        clangd: null,
      });
    } else if (w === undefined && c !== undefined) {
      infos.push({
        name: c.name,
        repository: c.repository,
        description: c.description,
        headerDescription: c.headerDescription,
        wandbox: null,
        clangd: {
          tagName: c.tagName,
          publishedAt: c.publishedAt,
        },
      });
    } else if (w !== undefined && c !== undefined) {
      // 基本 wandbox 側の方が新しいはずなので wandbox の情報を優先する
      infos.push({
        name: w.name,
        repository: w.repository,
        description: w.description,
        headerDescription: w.headerDescription,
        wandbox: {
          tagName: w.tagName,
          publishedAt: w.publishedAt,
        },
        clangd: {
          tagName: c.tagName,
          publishedAt: c.publishedAt,
        },
      });
    }
  }

  return infos;
}

export function createCompilerList(compilerListJson: AnyJson[]): CompilerList {
  const compilerInfos = compilerListJson.map(resolveCompilerInfo);
  const languages: { [lang: string]: CompilerInfo[] } = {};
  for (const info of compilerInfos) {
    if (info.language in languages) {
      languages[info.language].push(info);
    } else {
      languages[info.language] = [info];
    }
  }
  return {
    compilers: compilerInfos,
    languages: languages,
  };
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

export function redirectWithCookie(url: string, cookie: string): Response {
  return new Response(null, {
    status: 302,
    headers: {
      Location: url,
      "Set-Cookie": cookie,
    },
  });
}

export async function updateSession(env: Env, request: Request, permlinkId: string | null): Promise<{ cookie: string, githubUser: GithubUser, permlinkData: PermlinkData | null }> {
  const ss = getSessionStorage(env);
  const session = await ss.getSession(request.headers.get("Cookie"));

  const githubUser = session.has("github_user")
    ? JSON.parse(session.get("github_user"))
    : null;

  const permlinkData =
    permlinkId === null
      ? null
      : resolvePermlinkData(
        permlinkId,
        await fetchPermlinkData(env, permlinkId, request)
      );

  const cookie = await ss.commitSession(session, {
    maxAge: 30 * 24 * 60 * 60,
    sameSite: "lax",
  });
  return { cookie, githubUser, permlinkData };
}