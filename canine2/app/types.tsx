
export type GithubAccessToken = {
  access_token: string;
};

export type GithubUser = {
  id: number;
  login: string;
  avatar_url: string;
  html_url: string;
};

export type WandboxLoaderData = {
  githubUser: GithubUser | null;
  permlinkData: any;
  env: Env;
};
