import fs from "node:fs";
import {
  vitePlugin as remix,
  cloudflareDevProxyVitePlugin as remixCloudflareDevProxy,
} from "@remix-run/dev";
import { defineConfig } from "vite";
import tsconfigPaths from "vite-tsconfig-paths";

import { getLoadContext } from "./load-context";

declare module "@remix-run/cloudflare" {
  interface Future {
    v3_singleFetch: true;
  }
}

export default defineConfig({
  build: {
    minify: true,
    target: "esnext",
  },
  plugins: [
    remixCloudflareDevProxy({ getLoadContext }),
    remix({
      future: {
        v3_fetcherPersist: true,
        v3_relativeSplatPath: true,
        v3_throwAbortReason: true,
        v3_singleFetch: true,
        v3_lazyRouteDiscovery: true,
      },
    }),
    tsconfigPaths(),
    {
      name: "isolation",
      configureServer(server) {
        server.middlewares.use((req, res, next) => {
          res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
          res.setHeader("Cross-Origin-Embedder-Policy", "require-corp");
          if (req.url?.startsWith("/static/wasm/clangd.wasm")) {
            res.setHeader("Content-Type", "application/wasm");
            fs.createReadStream("public-r2/clangd.wasm").pipe(res);
            return;
          }
          next();
        });
      },
    },
  ],
  ssr: {
    resolve: {
      conditions: ["workerd", "worker", "browser"],
    },
  },
  resolve: {
    mainFields: ["browser", "module", "main"],
  },
  worker: {
    format: "es",
  },
  define: {
    __WASM_SIZE__: fs.statSync("public-r2/clangd.wasm").size,
  },
});
