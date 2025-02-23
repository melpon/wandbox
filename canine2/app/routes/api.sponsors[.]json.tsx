import { LoaderFunction } from "@remix-run/cloudflare";
import { fetchSponsorsData, withCors } from "~/utils/handleApi";

export const loader: LoaderFunction = async ({ request, context }) => {
    const env = context.cloudflare.env;
    const json = await fetchSponsorsData(env, request);
    let responseHeaders: HeadersInit = {};
    responseHeaders = withCors(responseHeaders, request);
    return Response.json(json, { headers: responseHeaders });
}
