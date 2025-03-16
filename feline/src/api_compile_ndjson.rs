use crate::{
    podman::run_streaming,
    types::{
        AppConfig, AppError, Code, CompileNdjsonResult, CompileParameter, Compiler, Config, Issuer,
        PodmanConfig, RunLog,
    },
    util::{base64_encode, make_issuer, make_random_str},
};
use axum::{
    Json,
    body::Body,
    extract::State,
    http::{HeaderMap, Uri},
    response::IntoResponse,
};
use std::{
    path::{Path, PathBuf},
    process::Output,
    sync::Arc,
};
use tokio::{
    fs::{File, create_dir_all},
    io::AsyncWriteExt as _,
    process::Command,
    sync::mpsc::{self, Receiver},
    time::Duration,
};
use tokio_stream::{StreamExt as _, wrappers::ReceiverStream};

async fn create_file_with_dirs(path: &Path, content: &[u8]) -> Result<(), anyhow::Error> {
    if let Some(parent) = path.parent() {
        create_dir_all(parent).await?;
    }
    let mut file: File = File::create(&path).await?;
    file.write_all(&content).await?;
    Ok(())
}

// 戻り値のパスが必ず dir の下にあることを保証する
// file_path が絶対パスだったり "../" みたいな文字列が含まれて dir の外を指定したらエラーになる
pub fn resolve_safe_path(dir: &Path, file_path: &Path) -> Result<PathBuf, anyhow::Error> {
    // 絶対パスは一括でアウト
    if file_path.is_absolute() {
        return Err(anyhow::anyhow!("Invalid path"));
    }
    // dir が空文字なのは多分設定を忘れてるのでエラーにしておく
    if dir
        .to_str()
        .ok_or(anyhow::anyhow!("Invalid dir"))?
        .is_empty()
    {
        return Err(anyhow::anyhow!("dir is empty"));
    }

    let full_path = dir.join(file_path);
    let mut full_path_normed: PathBuf = PathBuf::new();

    let mut components = full_path.components();
    while let Some(component) = components.next() {
        match component {
            std::path::Component::Normal(part) => {
                full_path_normed.push(part);
            }
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if !full_path_normed.pop() {
                    return Err(anyhow::anyhow!("Invalid path"));
                }
            }
            std::path::Component::RootDir => {
                full_path_normed.push("/");
            }
            _ => {
                return Err(anyhow::anyhow!("Invalid path"));
            }
        }
    }
    if full_path_normed.starts_with(dir) {
        return Ok(full_path_normed);
    } else {
        return Err(anyhow::anyhow!("Invalid path"));
    }
}

pub fn resolve_arguments(
    config: &Config,
    options: &str,
    option_raw: &str,
    runtime: bool,
) -> Vec<String> {
    let mut args: Vec<String> = vec![];
    // options は , 区切りのオプション
    for opt in options.split(',') {
        // switches に一致するオプションがあったら flags を追加
        if let Some(switches) = config.switches.get(opt) {
            if switches.runtime != runtime {
                continue;
            }
            args.extend(switches.flags.clone());
        }
    }
    // option_raw は \n 区切りのオプション
    let mut opts: Vec<&str> = option_raw.split('\n').collect();
    if !opts.is_empty() && opts[opts.len() - 1].is_empty() {
        opts.pop();
    }
    args.extend(opts.iter().map(|v| v.to_string()));
    args
}

fn get_compiler<'a>(config: &'a Config, name: &str) -> Result<&'a Compiler, anyhow::Error> {
    config
        .compilers
        .iter()
        .find(|v| &v.name == name)
        .ok_or(anyhow::anyhow!("Compiler not found"))
}

pub async fn prepare_environment(
    config: &Config,
    safe_run_dir: &str,
    timestamp: &chrono::DateTime<chrono::Local>,
    unique_name: &str,
    compiler: &str,
    code: &str,
    codes: &Vec<Code>,
    image: &str,
) -> Result<PathBuf, anyhow::Error> {
    let info: &Compiler = get_compiler(&config, &compiler)?;

    // ユーザーが実行するための環境を作る
    // ここは podman に守られてないので注意して実装する
    let base_dir: PathBuf = {
        let fmtstr: String = timestamp.format("%Y%m%d_%H%M%S").to_string();
        let dir: String = format!("wandbox_{}_{}", fmtstr, unique_name);
        Path::new(&safe_run_dir).join(&dir)
    };

    let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&info.output_file))?;
    create_file_with_dirs(&path, &code.as_bytes()).await?;
    for code in codes {
        let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&code.file))?;
        create_file_with_dirs(&path, &code.code.as_bytes()).await?;
    }

    // 以下のコマンドを実行してコンテナ内のユーザーが書き込めるようにする
    //
    // $ podman run --rm wandbox-runner id -u wandbox
    // 1001
    // $ podman run --rm wandbox-runner id -g wandbox
    // 1001
    // $ podman unshare chown -R 1001:1001 base_dir

    // まず uid と gid を取得
    let uid: Output = Command::new("podman")
        .arg("run")
        .arg("--rm")
        .arg(&image)
        .arg("id")
        .arg("-u")
        .arg("wandbox")
        .output()
        .await?;
    if !uid.status.success() {
        return Err(anyhow::anyhow!("Failed to get uid"));
    }
    let uid: String = String::from_utf8(uid.stdout)?.trim().to_string();
    let gid: Output = Command::new("podman")
        .arg("run")
        .arg("--rm")
        .arg(&image)
        .arg("id")
        .arg("-u")
        .arg("wandbox")
        .output()
        .await?;
    if !gid.status.success() {
        return Err(anyhow::anyhow!("Failed to get gid"));
    }
    let gid: String = String::from_utf8(gid.stdout)?.trim().to_string();
    let r: Output = Command::new("podman")
        .arg("unshare")
        .arg("chown")
        .arg("-R")
        .arg(format!("{}:{}", uid, gid))
        .arg(&base_dir)
        .output()
        .await?;
    if !r.status.success() {
        return Err(anyhow::anyhow!("Failed to unshare"));
    }
    Ok(base_dir)
}

pub async fn log_run_codes(
    config: &Config,
    safe_run_log_dir: &str,
    timestamp: &chrono::DateTime<chrono::Local>,
    unique_name: &str,
    compiler: &str,
    stdin: &[u8],
    compiler_option_raw: &str,
    runtime_option_raw: &str,
    compiler_options: &str,
    issuer: &Issuer,
    code: &str,
    codes: &Vec<Code>,
) -> Result<(), anyhow::Error> {
    let info: &Compiler = get_compiler(&config, &compiler)?;
    let run_log: RunLog = RunLog {
        compiler: compiler.to_string(),
        stdin: base64_encode(&stdin),
        compiler_option_raw: compiler_option_raw.to_string(),
        runtime_option_raw: runtime_option_raw.to_string(),
        compiler_options: compiler_options.to_string(),
        issuer: issuer.clone(),
    };
    let (base_base_dir, name) = {
        let date: String = timestamp.format("%Y%m%d").to_string();
        let fmtstr: String = timestamp.format("%Y%m%d_%H%M%S").to_string();
        let dir: String = format!("wandbox_{}_{}", fmtstr, unique_name);
        (Path::new(&safe_run_log_dir).join(&date), dir)
    };
    let json_file: String = name.clone() + ".json";
    let path: PathBuf = resolve_safe_path(&base_base_dir, &Path::new(&json_file))?;
    create_file_with_dirs(&path, &serde_json::to_string_pretty(&run_log)?.as_bytes()).await?;

    let base_dir: PathBuf = base_base_dir.join(&name);
    let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&info.output_file))?;
    create_file_with_dirs(&path, &code.as_bytes()).await?;
    for code in codes {
        let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&code.file))?;
        create_file_with_dirs(&path, &code.code.as_bytes()).await?;
    }

    Ok(())
}

pub fn run_compile_ndjson(
    config: Arc<AppConfig>,
    base_dir: PathBuf,
    body: CompileParameter,
) -> Receiver<CompileNdjsonResult> {
    // プログラムを実行していく
    let (tx, rx) = mpsc::channel::<CompileNdjsonResult>(100);
    tokio::spawn(async move {
        // 気軽に await? できるようにしてるだけ
        let r: Result<(), anyhow::Error> = async move {
            let info: &Compiler = get_compiler(&config.config, &body.compiler)?;
            let args: Vec<String> = resolve_arguments(
                &config.config,
                &body.options,
                &body.compiler_option_raw,
                false,
            );
            let base_dir: PathBuf = base_dir.canonicalize()?;
            let base_dir_str: &str = base_dir.to_str().ok_or(anyhow::anyhow!("Invalid path"))?;
            let podman_config: PodmanConfig =
                config.podman.with_workdir(base_dir_str, "/home/wandbox");

            tx.send(CompileNdjsonResult {
                r#type: "Control".to_string(),
                data: b"Start".to_vec(),
            })
            .await?;

            // コンパイルフェーズ
            let mut crx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
                &podman_config,
                &info.compile_command[0],
                info.compile_command[1..]
                    .iter()
                    .cloned()
                    .chain(args)
                    .collect(),
                b"".to_vec(),
                Duration::from_secs(60),
            );
            let mut cont = true;
            while let Some(v) = crx.recv().await {
                // StdOut と StdErr を CompilerMessageS と CompilerMessageE に変換する
                let v = match v.r#type.as_str() {
                    "StdOut" => CompileNdjsonResult {
                        r#type: "CompilerMessageS".to_string(),
                        data: v.data,
                    },
                    "StdErr" => CompileNdjsonResult {
                        r#type: "CompilerMessageE".to_string(),
                        data: v.data,
                    },
                    _ => v,
                };
                // Signal か、ExitCode の 0 以外だったらコンパイルフェーズで終了
                if v.r#type == "Signal" || (v.r#type == "ExitCode" && v.data != b"0") {
                    cont = false;
                }
                // ExitCode 0 は送信しない
                if v.r#type == "ExitCode" && v.data == b"0" {
                    continue;
                }
                tx.send(v).await?;
            }
            if !cont {
                return Ok(());
            }

            // 実行フェーズ
            let args: Vec<String> = resolve_arguments(
                &config.config,
                &body.options,
                &body.runtime_option_raw,
                true,
            );
            let mut rrx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
                &podman_config,
                &info.run_command[0],
                info.run_command[1..].iter().cloned().chain(args).collect(),
                body.stdin,
                Duration::from_secs(60),
            );
            while let Some(v) = rrx.recv().await {
                tx.send(v).await?;
            }

            tx.send(CompileNdjsonResult {
                r#type: "Control".to_string(),
                data: b"Finish".to_vec(),
            })
            .await?;

            Ok(())
        }
        .await;

        if let Err(e) = r {
            log::error!("Error {}", e);
        }
    });
    rx
}

pub async fn post_api_compile_ndjson(
    uri: Uri,
    headers: HeaderMap,
    State(config): State<Arc<AppConfig>>,
    Json(body): Json<CompileParameter>,
) -> Result<impl IntoResponse, AppError> {
    let issuer: Issuer = make_issuer(uri.path(), &headers, &body.github_user)?;
    let now: chrono::DateTime<chrono::Local> = chrono::Local::now();
    let unique_name: String = make_random_str(6);

    log_run_codes(
        &config.config,
        &config.safe_run_log_dir,
        &now,
        &unique_name,
        &body.compiler,
        &body.stdin,
        &body.compiler_option_raw,
        &body.runtime_option_raw,
        &body.options,
        &issuer,
        &body.code,
        &body.codes,
    )
    .await?;

    let base_dir: PathBuf = prepare_environment(
        &config.config,
        &config.safe_run_dir,
        &now,
        &unique_name,
        &body.compiler,
        &body.code,
        &body.codes,
        &config.podman.image,
    )
    .await?;

    let rx: Receiver<CompileNdjsonResult> = run_compile_ndjson(config.clone(), base_dir, body);
    let stream = ReceiverStream::new(rx).map(|v| serde_json::to_string(&v).map(|v| v + "\n"));
    Ok(Body::from_stream(stream))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AppConfig, create_app,
        types::{Code, CompilerInfo, Config, PodmanConfig},
    };
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode, header},
    };
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_resolve_safe_path() {
        // dir が絶対パス
        {
            let dir = Path::new("/tmp");

            let file_path: &Path = Path::new("foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/foo/bar"));

            let file_path: &Path = Path::new("../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("../../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("foo/../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/bar"));

            let file_path: &Path = Path::new("foo/.././././../tmp/../tmp/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/foo/bar"));

            let file_path: &Path = Path::new("/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("/tmp/foo");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");
        }

        // dir が相対パス
        {
            let dir: &Path = Path::new("tmp");

            let file_path: &Path = Path::new("foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/foo/bar"));

            let file_path: &Path = Path::new("../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("../../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("foo/../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/bar"));

            let file_path: &Path = Path::new("foo/.././././../tmp/../tmp/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/foo/bar"));

            let file_path: &Path = Path::new("/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("/tmp/foo");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");
        }
    }

    struct RemoveDirGuard {
        path: PathBuf,
    }
    impl Drop for RemoveDirGuard {
        fn drop(&mut self) {
            if self.path.exists() {
                // 一部のディレクトリを podman 内ユーザーにしてるので、
                // podman unshare を使ってうまいこと消す必要がある
                let _ = std::process::Command::new("bash")
                    .arg("-c")
                    .arg(format!(
                        "podman unshare chown -R $(id -u):$(id -g) {} && podman unshare rm -rf {}",
                        self.path.to_str().unwrap(),
                        self.path.to_str().unwrap()
                    ))
                    .output()
                    .unwrap();
            }
        }
    }

    #[tokio::test]
    async fn test_post_api_compile_ndjson() {
        let base_dir: String = format!("_tmp/{}", make_random_str(8));
        let safe_run_dir: String = format!("{}/jail", &base_dir);
        let safe_run_log_dir: String = format!("{}/log", &base_dir);
        let _guard: RemoveDirGuard = RemoveDirGuard {
            path: base_dir.clone().into(),
        };

        let config: Config = serde_json::from_reader(
            std::fs::File::open("src/tests/data/api_compile_input.json").unwrap(),
        )
        .unwrap();
        let podman_config: PodmanConfig = PodmanConfig::new("wandbox-runner");
        let app_config: AppConfig = AppConfig {
            podman: podman_config.clone(),
            config: config.clone(),
            safe_run_dir: safe_run_dir.clone(),
            safe_run_log_dir: safe_run_log_dir.clone(),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let body: CompileParameter = CompileParameter {
            compiler: "test".to_string(),
            code: "echo Hello && touch test && cat subfile".to_string(),
            codes: vec![Code {
                file: "subfile".to_string(),
                code: "Subfile".to_string(),
            }],
            options: "warning".to_string(),
            stdin: b"".to_vec(),
            compiler_option_raw: "".to_string(),
            runtime_option_raw: "".to_string(),
            github_user: "".to_string(),
            title: "".to_string(),
            description: "".to_string(),
            save: false,
            created_at: 0,
            is_private: false,
            compiler_info: CompilerInfo {
                ..CompilerInfo::default()
            },
        };
        let request: Request<Body> = Request::builder()
            .method("POST")
            .uri("/api/compile.ndjson")
            .header(header::CONTENT_TYPE, "application/json")
            .body(Body::from(serde_json::to_string(&body).unwrap()))
            .unwrap();

        let response: Response<Body> = app.oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);

        // レスポンスボディの確認
        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        assert!(!body.is_empty());
        let json: Vec<CompileNdjsonResult> = body
            .split(|b| *b == b'\n')
            .filter(|v| !v.is_empty())
            .map(|v| serde_json::from_slice(&v).unwrap())
            .collect();

        assert_eq!(json.len(), 5);
        assert_eq!(json[0].r#type, "Control");
        assert_eq!(json[0].data, b"Start");
        // コンパイル時のコマンドは、引数に渡されたオプションをそのまま表示する内容になっている
        // warning を指定したので -Wall -Wextra が表示される
        assert_eq!(json[1].r#type, "CompilerMessageS");
        assert_eq!(json[1].data, b"-Wall -Wextra\n");
        // 実行時は code に書いた内容を bash で実行するだけ
        // code の内容は echo Hello; cat subfile なので、Hello と Subfile が表示される
        assert_eq!(json[2].r#type, "StdOut");
        assert_eq!(json[2].data, b"Hello\nSubfile");
        assert_eq!(json[3].r#type, "ExitCode");
        assert_eq!(json[3].data, b"0");
        assert_eq!(json[4].r#type, "Control");
        assert_eq!(json[4].data, b"Finish");
    }
}
