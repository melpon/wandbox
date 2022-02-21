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
  useParams,
} from "remix";
import type { MetaFunction, LinksFunction } from "remix";

import wandboxStyles from "./styles/wandbox.css";
import { getSession, commitSession } from "./sessions.server";
import { fetchPermlinkData } from "./entry.server";
import type { PermlinkData } from "./hooks/permlink";
import { resolvePermlinkData } from "./hooks/permlink";

export const meta: MetaFunction = ({ params, data }) => {
  let title: string;
  const permlinkData: PermlinkData | null = data.permlinkData;

  if (permlinkData === null) {
    title = "Wandbox";
  } else {
    title = `[${permlinkData.parameter.compilerInfo.language}] ${permlinkData.parameter.title} - Wandbox`;
  }
  return { title };
};

export const links: LinksFunction = () => {
  return [{ rel: "stylesheet", href: wandboxStyles }];
};

export const loader: LoaderFunction = async ({ request, params }) => {
  const session = await getSession(request.headers.get("Cookie"));

  const githubUser = session.has("github_user")
    ? JSON.parse(session.get("github_user"))
    : null;

  const { permlinkId } = params;
  const permlinkData =
    permlinkId === undefined
      ? null
      : resolvePermlinkData(permlinkId, await fetchPermlinkData(permlinkId));

  const data: WandboxLoaderData = { githubUser, permlinkData };

  return json(data, {
    headers: {
      "Set-Cookie": await commitSession(session),
    },
  });
};

export default function App() {
  const data: WandboxLoaderData = useLoaderData();
  // permlinkData はタイトルの設定のために用意しているだけなので HTML 生成時には除ける
  const data2: WandboxLoaderData = {
    githubUser: data.githubUser,
    permlinkData: null,
  };
  return (
    <html>
      <head>
        <meta charSet="utf-8" />
        <meta
          name="viewport"
          content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=0"
        />
        <Meta />
        <Links />
      </head>
      <body>
        <Outlet />
        <ScrollRestoration />
        <script
          dangerouslySetInnerHTML={{
            __html: `
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
