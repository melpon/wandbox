use crate::{
    jail::{log_run_codes, prepare_environment, run_compile_ndjson},
    types::{AppConfig, AppError, CompileNdjsonResult, CompileParameter, Issuer},
    util::{make_issuer, make_random_str},
};
use axum::{
    Json,
    body::Body,
    extract::State,
    http::{HeaderMap, Uri},
    response::IntoResponse,
};
use std::{path::PathBuf, sync::Arc};
use tokio::sync::mpsc::Receiver;
use tokio_stream::{StreamExt as _, wrappers::ReceiverStream};

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
