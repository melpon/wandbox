import { LoaderFunction } from "@remix-run/cloudflare";
import { Provider } from "react-redux";

import { Wandbox } from "~/components/Wandbox";
import store from "~/store";
import { WandboxLoaderData } from "~/types";
import { updateSession } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, params, context }) => {
  const env = context.cloudflare.env;
  const { permlinkId } = params;
  const { cookie, githubUser, permlinkData } = await updateSession(env, request, permlinkId ?? null)

  const data: WandboxLoaderData = { githubUser, permlinkData, env: context.cloudflare.env };

  return Response.json(data, {
    headers: {
      "Set-Cookie": cookie,
    },
  });
};

export default function Permlink() {
  return (
    <Provider store={store}>
      <Wandbox />
    </Provider>
  );
}
