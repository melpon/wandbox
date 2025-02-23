import { ActionFunction, AppLoadContext, LoaderFunction } from "@remix-run/cloudflare";
import { withClientIP, withCors } from "~/utils/handleApi";

async function actionAndLoader(request: Request, context: AppLoadContext): Promise<Response> {
    const url = new URL(request.url);
    const env = context.cloudflare.env;
    let json: any = null;
    if (request.method === "POST") {
      json = await request.json();
    }
    const headers = withClientIP(
      {
        "Content-Type": "application/json",
        Accept: "application/json",
      },
      request
    );
    const resp = await fetch(`${env.WANDBOX_URL_PREFIX}${url.pathname}`, {
      method: request.method,
      headers: headers,
      body: json === null ? undefined : JSON.stringify(json),
    });
    let responseHeaders: HeadersInit = resp.headers;
    responseHeaders = withCors(responseHeaders, request);
    return new Response(await resp.text(), {
      headers: responseHeaders,
      status: resp.status,
      statusText: resp.statusText,
    });
}

export const loader: LoaderFunction = async ({ request, context }) => {
    return actionAndLoader(request, context);
}

export const action: ActionFunction = async ({ request, context }) => {
    return actionAndLoader(request, context);
}
