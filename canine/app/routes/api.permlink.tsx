import { ActionFunction, LoaderFunction } from "@remix-run/cloudflare";
import { getSessionStorage } from "~/sessions.server";
import { withClientIP, withCors } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request }) => {
  if (request.method === "OPTIONS") {
    return new Response(null, {
      status: 204,
      headers: withCors({}, request),
    });
  }
  return new Response(null, { status: 404 });
}

export const action: ActionFunction = async ({ request, context }) => {
  const env = context.cloudflare.env;
  const ss = getSessionStorage(env);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
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