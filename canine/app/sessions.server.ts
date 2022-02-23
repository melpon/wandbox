import { createCookie, createCloudflareKVSessionStorage } from "remix";

const sessionCookie = createCookie("__session", {
  secrets: [WANDBOX_KV_COOKIE_SECRET],
  sameSite: true,
});

const { getSession, commitSession, destroySession } =
  createCloudflareKVSessionStorage({
    kv: KV_SESSION,
    cookie: sessionCookie,
  });

export { getSession, commitSession, destroySession };
