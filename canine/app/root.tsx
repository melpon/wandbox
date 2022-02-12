import {
  json,
  Links,
  LiveReload,
  LoaderFunction,
  Meta,
  Outlet,
  Scripts,
  ScrollRestoration,
  useLoaderData,
} from "remix";
import type { MetaFunction, LinksFunction } from "remix";
import wandboxStyles from "./styles/wandbox.css";
import { getSession, commitSession } from "./sessions.server";

export const meta: MetaFunction = () => {
  return { title: "New Remix App" };
};

export const links: LinksFunction = () => {
  return [{ rel: "stylesheet", href: wandboxStyles }];
};

export const loader: LoaderFunction = async ({ request }) => {
  const session = await getSession(request.headers.get("Cookie"));

  const githubUser = session.has("github_user")
    ? JSON.parse(session.get("github_user"))
    : null;
  const data: WandboxLoaderData = { githubUser };
  return json(data, {
    headers: {
      "Set-Cookie": await commitSession(session),
    },
  });
};

export default function App() {
  const data: WandboxLoaderData = useLoaderData() || { githubUser: null };
  return (
    <html>
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width,initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body>
        <Outlet />
        <ScrollRestoration />
        <script
          dangerouslySetInnerHTML={{
            __html: `
              window.WANDBOX_URL_PREFIX = ${JSON.stringify(WANDBOX_URL_PREFIX)};
              window.WANDBOX_GITHUB_CLIENT_ID = ${JSON.stringify(
                WANDBOX_GITHUB_CLIENT_ID
              )};
              window.WANDBOX_LOADER_DATA = ${JSON.stringify(data)}
            `,
          }}
        />
        <Scripts />
        <script
          src="https://platform.twitter.com/widgets.js"
          type="text/javascript"
        />
        {process.env.NODE_ENV === "development" && <LiveReload />}
      </body>
    </html>
  );
}
