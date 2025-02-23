import { LoaderFunction } from "@remix-run/cloudflare";
import { fetchListData, withCors } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, context }) => {
    const env = context.cloudflare.env;
    const json = await fetchListData(env, request);
    var responseHeaders: HeadersInit = {};
    responseHeaders = withCors(responseHeaders, request);
    return Response.json(json, { headers: responseHeaders });
}
