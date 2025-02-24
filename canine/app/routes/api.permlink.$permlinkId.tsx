import { LoaderFunction } from "@remix-run/cloudflare";
import { fetchPermlinkData, withCors } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, params, context }) => {
  if (params.permlinkId === undefined) {
    return Response.error();
  }
  const env = context.cloudflare.env;
  const json = await fetchPermlinkData(env, params.permlinkId, request);
  let responseHeaders: HeadersInit = {};
  responseHeaders = withCors(responseHeaders, request);
  return Response.json(json, { headers: responseHeaders });
}
