import { LoaderFunction } from "@remix-run/cloudflare";
import { Provider } from "react-redux";

import { Wandbox } from "~/components/Wandbox";
import { Hpplib } from "~/features/slice";
import { AnyJson } from "~/hooks/fetch";
import store from "~/store";
import { WandboxLoaderData } from "~/types";
import { createCompilerList, fetchHpplibData, fetchListData, fetchSponsorsData, updateSession, mergeHpplib } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, params, context }) => {
  const env = context.cloudflare.env;
  const { permlinkId } = params;
  const { cookie, githubUser, permlinkData } = await updateSession(env, request, permlinkId ?? null)
  const sponsors = await fetchSponsorsData(env, request);
  const compilerListJson = await fetchListData(env, request);
  const compilerList = createCompilerList(compilerListJson as AnyJson[]);
  const hpplibJson = await fetchHpplibData(env, request);
  const hpplib = mergeHpplib(hpplibJson as unknown as Hpplib);

  const data: WandboxLoaderData = { githubUser, compilerList, sponsors, permlinkData, hpplib, env: context.cloudflare.env };

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
