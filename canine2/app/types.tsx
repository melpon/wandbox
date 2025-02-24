
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
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  permlinkData: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  sponsors: any;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  compilerList: any;
  env: Env;
};
