import type { MetaFunction, MetaDescriptor, LinksFunction, LoaderFunction } from "@remix-run/cloudflare";
import {
  Links,
  Meta,
  Outlet,
  Scripts,
  ScrollRestoration,
} from "@remix-run/react";

import "./tailwind.css";
import "./styles/wandbox.scss";

//import wandboxStyles from "./styles/wandbox.css";
import { getSessionStorage } from "./sessions.server";
import { fetchPermlinkData } from "./entry.server";
import type { PermlinkData } from "./hooks/permlink";
import { resolvePermlinkData } from "./hooks/permlink";
import type { WandboxLoaderData } from "~/types";

export const loader: LoaderFunction = async ({ request, params, context }) => {
  const env = context.cloudflare.env;
  const ss = getSessionStorage(env);
  const session = await ss.getSession(request.headers.get("Cookie"));

  const githubUser = session.has("github_user")
    ? JSON.parse(session.get("github_user"))
    : null;

  const { permlinkId } = params;
  const permlinkData =
    permlinkId === undefined
      ? null
      : resolvePermlinkData(
          permlinkId,
          await fetchPermlinkData(env, permlinkId, request)
        );

  const data: WandboxLoaderData = { githubUser, permlinkData, env: context.cloudflare.env };

  const cookie = await ss.commitSession(session, {
    maxAge: 30 * 24 * 60 * 60,
    sameSite: "lax",
  });
  return Response.json(data, {
    headers: {
      "Set-Cookie": cookie,
    },
  });
};

export const meta: MetaFunction = ({ params, data }) => {
  let title: string;
  // const permlinkData: PermlinkData | null = (data as any).permlinkData;
  const permlinkData = null;

  if (permlinkData === null) {
    title = "Wandbox";
  } else {
    if (permlinkData.parameter.title.length !== 0) {
      title = `[${permlinkData.parameter.compilerInfo.language}] ${permlinkData.parameter.title} - Wandbox`;
    } else {
      title = `[${permlinkData.parameter.compilerInfo.language}] ${permlinkData.parameter.compilerInfo.displayName} ${permlinkData.parameter.compilerInfo.version} - Wandbox`;
    }
  }

  const result: MetaDescriptor = {};
  result.title = title;

  // Twitter カードの設定
  if (permlinkData !== null) {
    result["twitter:card"] = "summary";
    result["twitter:title"] = title;
    result["twitter:description"] = permlinkData.parameter.code.slice(0, 100);
  }
  return [result];
};

// export const links: LinksFunction = () => {
//   return [{ rel: "stylesheet", href: wandboxStyles }];
// };
export const links: LinksFunction = () => [
  { rel: "preconnect", href: "https://fonts.googleapis.com" },
  {
    rel: "preconnect",
    href: "https://fonts.gstatic.com",
    crossOrigin: "anonymous",
  },
  {
    rel: "stylesheet",
    href: "https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap",
  },
  //{ rel: "stylesheet", href: wandboxStyles },
];


export function Layout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <head>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body>
        {children}
        <ScrollRestoration />
        <Scripts />
        <script
          src="https://platform.twitter.com/widgets.js"
          type="text/javascript"
        />
      </body>
    </html>
  );
}

export default function App() {
  return <Outlet />;
}
