import { createCookie, createWorkersKVSessionStorage, SessionStorage, SessionData } from "@remix-run/cloudflare";

export function getSessionStorage(env: Env): SessionStorage<SessionData, SessionData> {
  return createWorkersKVSessionStorage({
    kv: env.KV_SESSION, // Cloudflare KV
    cookie: createCookie("session", {
      secrets: [env.WANDBOX_KV_COOKIE_SECRET], // シークレットキーを環境変数から取得
      secure: true,
      httpOnly: true,
      path: "/",
      sameSite: "lax",
    }),
  });
}
