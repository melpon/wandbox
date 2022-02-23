/// <reference types="@remix-run/dev" />
/// <reference types="@remix-run/cloudflare-workers/globals" />
/// <reference types="@cloudflare/workers-types" />

declare type GithubUser = {
  id: number;
  login: string;
  avatar_url: string;
  html_url: string;
};

declare interface WandboxLoaderData {
  githubUser: GithubUser | null;
  permlinkData: any;
}

declare var WANDBOX_URL_PREFIX: string;
declare var WANDBOX_GITHUB_CLIENT_ID: string;
declare var WANDBOX_GITHUB_CLIENT_SECRET: string;
declare var WANDBOX_KV_COOKIE_SECRET: string;
declare var WANDBOX_LOADER_DATA: WandboxLoaderData;
declare var KV_SESSION: KVNamespace;
declare var KV_CACHE: KVNamespace;
