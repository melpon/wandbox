import { LoaderFunction } from "@remix-run/cloudflare";
import { getSessionStorage } from "~/sessions.server";
import { redirectWithCookie, getGithubAccessToken, getGithubUser } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, context }) => {
  // GitHub 認証
  const url = new URL(request.url);
  const env = context.cloudflare.env;
  const ss = getSessionStorage(env);
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
