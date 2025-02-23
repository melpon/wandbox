import { LoaderFunction } from "@remix-run/cloudflare";
import { getSessionStorage } from "~/sessions.server";
import { redirectWithCookie } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, context }) => {
  // ログアウト
  const url = new URL(request.url);
  const env = context.cloudflare.env;
  const ss = getSessionStorage(env);
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
