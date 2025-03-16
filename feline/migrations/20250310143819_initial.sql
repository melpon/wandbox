-- Add migration script here
CREATE TABLE IF NOT EXISTS code (
  id                   INTEGER PRIMARY KEY,
  compiler             VARCHAR NOT NULL,
  code                 VARCHAR NOT NULL,
  optimize             BOOLEAN NOT NULL,
  warning              BOOLEAN NOT NULL,
  options              VARCHAR NOT NULL,
  compiler_option_raw  VARCHAR NOT NULL DEFAULT "",
  runtime_option_raw   VARCHAR NOT NULL DEFAULT "",
  stdin                VARCHAR NOT NULL DEFAULT "",
  created_at           TIMESTAMP NOT NULL,
  title                VARCHAR NOT NULL DEFAULT "",
  description          VARCHAR NOT NULL DEFAULT "",
  github_user          VARCHAR NOT NULL DEFAULT "",
  private              BOOLEAN NOT NULL DEFAULT 0,
  updated_at           TIMESTAMP,
  CONSTRAINT unique_code UNIQUE (compiler, code, optimize, warning, created_at)
);
CREATE INDEX IF NOT EXISTS github_user_list ON code (github_user, private, created_at DESC);

CREATE TABLE IF NOT EXISTS codes (
  id                   INTEGER PRIMARY KEY,
  code_id              INTEGER NOT NULL REFERENCES code,
  "order"              INTEGER NOT NULL,
  file                 VARCHAR NOT NULL,
  code                 VARCHAR NOT NULL,
  CONSTRAINT unique_codes UNIQUE (code_id, "order")
);

CREATE TABLE IF NOT EXISTS compiler_info (
  id                   INTEGER PRIMARY KEY,
  code_id              INTEGER NOT NULL REFERENCES code,
  json                 VARCHAR NOT NULL,
  CONSTRAINT unique_compiler_info UNIQUE (code_id)
);

CREATE TABLE IF NOT EXISTS link (
  id           INTEGER PRIMARY KEY,
  permlink     VARCHAR NOT NULL,
  code_id      INTEGER NOT NULL REFERENCES code,
  CONSTRAINT unique_link UNIQUE (permlink)
);
CREATE INDEX IF NOT EXISTS link_code ON link (code_id);

CREATE TABLE IF NOT EXISTS link_output (
  id           INTEGER PRIMARY KEY,
  link_id      INTEGER NOT NULL REFERENCES link,
  "order"      INTEGER NOT NULL,
  type         VARCHAR NOT NULL,
  output       VARCHAR NOT NULL,
  CONSTRAINT unique_link_output UNIQUE (link_id, "order")
);

CREATE TABLE IF NOT EXISTS github_user (
  id                    INTEGER PRIMARY KEY,
  username              VARCHAR NOT NULL,
  created_at            TIMESTAMP NOT NULL,
  updated_at            TIMESTAMP NOT NULL,
  github_access_token   VARCHAR NOT NULL DEFAULT "",
  wandbox_access_token  VARCHAR NOT NULL DEFAULT "",
  CONSTRAINT unique_name UNIQUE (username)
);
CREATE INDEX IF NOT EXISTS github_user_wandbox_access_token ON github_user (wandbox_access_token);
