import { createRequestHandler, type ServerBuild } from "@remix-run/cloudflare";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore This file won’t exist if it hasn’t yet been built
import * as build from "./build/server"; // eslint-disable-line import/no-unresolved
import { getLoadContext } from "./load-context";

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const handleRemixRequest = createRequestHandler(build as any as ServerBuild);

export default {
  async fetch(request, env, ctx) {
    try {
      const url = new URL(request.url);
      let response: Response;
      if (url.pathname == "/favicon.ico" || url.pathname.startsWith("/assets") || url.pathname.startsWith("/static")) {
        if (url.pathname.endsWith("clangd.wasm")) {
          // clangd.wasm は R2 から配信する
          const r = await env.R2_BUCKET.get(env.WANDBOX_R2_PREFIX + "clangd.wasm");
          if (r === null) {
            return new Response("Not found", { status: 404 });
          }
          response = new Response(r.body, { status: 200 });
        } else {
          response = await env.ASSETS.fetch(request);
        }
      } else {
        const loadContext = getLoadContext({
          request,
          context: {
            cloudflare: {
              // This object matches the return value from Wrangler's
              // `getPlatformProxy` used during development via Remix's
              // `cloudflareDevProxyVitePlugin`:
              // https://developers.cloudflare.com/workers/wrangler/api/#getplatformproxy
              cf: request.cf,
              ctx: {
                waitUntil: ctx.waitUntil.bind(ctx),
                passThroughOnException: ctx.passThroughOnException.bind(ctx),
              },
              caches,
              env,
            },
          },
        });
        response = await handleRemixRequest(request, loadContext);
      }

      // 必要なファイルに COEP と COOP を設定する
      const headers = new Headers(response.headers);
      if (url.pathname == "/" || url.pathname.startsWith("/permlink") ||
        (url.pathname.startsWith("/assets/main.worker") && url.pathname.endsWith(".js")) ||
        url.pathname.startsWith("/static/wasm/")) {
        headers.append("Cross-Origin-Opener-Policy", "same-origin");
        headers.append("Cross-Origin-Embedder-Policy", "require-corp");
      }
      return new Response(response.body, { status: response.status, headers: headers });
    } catch (error) {
      console.log(error);
      return new Response("An unexpected error occurred", { status: 500 });
    }
  },
} satisfies ExportedHandler<Env>;
