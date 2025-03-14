use crate::{
    api_compile_ndjson::{prepare_environment, run_compile_ndjson},
    config::convert_config_to_compiler_info,
    db::make_permlink,
    types::{
        AppConfig, CompileNdjsonResult, CompileParameter, CompileResult, CompilerInfo,
        PostPermlinkRequest,
    },
    util::{make_permlink_request, make_random_str, merge_compile_result},
};
use anyhow::Result;
use axum::{Json, extract::State, http::StatusCode};
use std::{path::PathBuf, sync::Arc};
use tokio::sync::mpsc::Receiver;

pub async fn post_api_compile_json(
    State(config): State<Arc<AppConfig>>,
    Json(body): Json<CompileParameter>,
) -> Result<Json<CompileResult>, (StatusCode, String)> {
    let compiler_infos: Vec<CompilerInfo> =
        convert_config_to_compiler_info(&config.config, &config.version_info);
    let compiler_info: &CompilerInfo = compiler_infos
        .iter()
        .find(|c| c.name == body.compiler)
        .ok_or((StatusCode::BAD_REQUEST, "Unknown compiler".to_string()))?;

    let r: Result<PathBuf> = prepare_environment(
        &config.config,
        &config.safe_run_dir,
        &body.compiler,
        &body.code,
        &body.codes,
    )
    .await;
    let base_dir: PathBuf = r.map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    let mut rx: Receiver<CompileNdjsonResult> =
        run_compile_ndjson(config.clone(), base_dir, body.clone());
    let mut results: Vec<CompileNdjsonResult> = Vec::new();
    while let Some(result) = rx.recv().await {
        results.push(result);
    }
    let mut result: CompileResult = merge_compile_result(&results);
    if body.save {
        let preq: PostPermlinkRequest = make_permlink_request(&body, &results);
        let permlink_name: String = make_random_str(16);
        make_permlink(
            &config.sqlite.clone().unwrap(),
            &permlink_name,
            &preq,
            &compiler_info,
        )
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
        result.permlink = permlink_name.clone();
        result.url = format!("{}permlink/{}", config.wandbox_url, permlink_name);
    }
    Ok(Json(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AppConfig, create_app,
        types::{Code, CompilerInfo, Config, GetPermlinkResponse, PodmanConfig, Switch},
    };
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode, header},
    };
    use http_body_util::BodyExt;
    use sqlx::{SqlitePool, migrate::Migrator};
    use std::{collections::HashMap, path::Path};
    use tower::ServiceExt;

    async fn setup_test_db() -> SqlitePool {
        let pool: SqlitePool = SqlitePool::connect(":memory:")
            .await
            .expect("Failed to create in-memory DB");

        // マイグレーションを適用
        let migrator: Migrator = Migrator::new(Path::new("./migrations"))
            .await
            .expect("Failed to load migrations");
        migrator
            .run(&pool)
            .await
            .expect("Failed to apply migrations");

        pool
    }

    struct RemoveDirGuard {
        path: PathBuf,
    }
    impl Drop for RemoveDirGuard {
        fn drop(&mut self) {
            if self.path.exists() {
                std::fs::remove_dir_all(&self.path).expect("Failed to remove test dir");
            }
        }
    }

    #[tokio::test]
    async fn test_post_api_compile_json() {
        let sqlite: SqlitePool = setup_test_db().await;
        let safe_run_dir: String = format!("_tmp/{}", make_random_str(8));
        let _guard: RemoveDirGuard = RemoveDirGuard {
            path: safe_run_dir.clone().into(),
        };

        let config: Config = serde_json::from_reader(
            std::fs::File::open("src/tests/data/api_compile_input.json").unwrap(),
        )
        .unwrap();
        let podman_config: PodmanConfig = PodmanConfig::new("wandbox-runner");
        let app_config: AppConfig = AppConfig {
            sqlite: Some(Arc::new(sqlite)),
            podman: podman_config.clone(),
            config: config.clone(),
            safe_run_dir: safe_run_dir.clone(),
            wandbox_url: "https://wandbox.org/".to_string(),
            version_info: HashMap::from([("test".to_string(), "0.1.2".to_string())]),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let req: CompileParameter = CompileParameter {
            compiler: "test".to_string(),
            code: "echo Hello; cat subfile".to_string(),
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
            .uri("/api/compile.json")
            .header(header::CONTENT_TYPE, "application/json")
            .body(Body::from(serde_json::to_string(&req).unwrap()))
            .unwrap();

        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();

        // ステータスコードの確認
        assert_eq!(response.status(), StatusCode::OK);

        // レスポンスボディの確認
        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        assert!(!body.is_empty());
        let json: CompileResult = serde_json::from_slice(&body).unwrap();
        assert_eq!(
            json,
            CompileResult {
                status: "0".to_string(),
                signal: "".to_string(),
                compiler_output: b"-Wall -Wextra\n".to_vec(),
                compiler_error: vec![],
                compiler_message: b"-Wall -Wextra\n".to_vec(),
                program_output: b"Hello\nSubfile".to_vec(),
                program_error: vec![],
                program_message: b"Hello\nSubfile".to_vec(),
                permlink: "".to_string(),
                url: "".to_string(),
            }
        );

        // セーブ付き
        let req: CompileParameter = CompileParameter { save: true, ..req };
        let request: Request<Body> = Request::builder()
            .method("POST")
            .uri("/api/compile.json")
            .header(header::CONTENT_TYPE, "application/json")
            .body(Body::from(serde_json::to_string(&req).unwrap()))
            .unwrap();
        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        assert!(!body.is_empty());
        println!("{:?}", String::from_utf8_lossy(&body.to_vec()));
        let json: CompileResult = serde_json::from_slice(&body).unwrap();
        // permlink と url 以外は同じ
        assert_eq!(
            CompileResult {
                permlink: "".to_string(),
                url: "".to_string(),
                ..json
            },
            CompileResult {
                status: "0".to_string(),
                signal: "".to_string(),
                compiler_output: b"-Wall -Wextra\n".to_vec(),
                compiler_error: vec![],
                compiler_message: b"-Wall -Wextra\n".to_vec(),
                program_output: b"Hello\nSubfile".to_vec(),
                program_error: vec![],
                program_message: b"Hello\nSubfile".to_vec(),
                permlink: "".to_string(),
                url: "".to_string(),
            }
        );
        assert_eq!(json.permlink.len(), 16);
        assert_eq!(
            json.url,
            "https://wandbox.org/permlink/".to_string() + &json.permlink
        );

        // ついでに /api/permlink/<permlink_id> で取得できるか確認
        let permlink: String = json.permlink.clone();
        let request: Request<Body> = Request::builder()
            .uri("/api/permlink/".to_string() + &permlink)
            .body(Body::empty())
            .unwrap();
        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        assert!(!body.is_empty());
        let json: GetPermlinkResponse = serde_json::from_slice(&body).unwrap();

        let p: CompileParameter = json.parameter.clone();
        assert_eq!(
            CompileParameter { created_at: 0, ..p },
            CompileParameter {
                compiler: "test".to_string(),
                code: "echo Hello; cat subfile".to_string(),
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
                    name: "test".to_string(),
                    version: "0.1.2".to_string(),
                    language: "Bash".to_string(),
                    display_name: "test compiler".to_string(),
                    templates: vec![],
                    compiler_option_raw: true,
                    runtime_option_raw: false,
                    display_compile_command: "test prog.sh".to_string(),
                    switches: vec![Switch::Single {
                        name: "warning".to_string(),
                        display_name: "Warnings".to_string(),
                        display_flags: "-- -Wall -Wextra".to_string(),
                        default: false,
                    },],
                },
            },
        );
        assert_ne!(p.created_at, 0);

        // results と result は /api/compile.ndjson と /api/compile.json の結果と同じになる

        let rs: Vec<CompileNdjsonResult> = json.results.clone();
        assert_eq!(rs.len(), 5);
        assert_eq!(rs[0].r#type, "Control");
        assert_eq!(rs[0].data, b"Start");
        assert_eq!(rs[1].r#type, "CompilerMessageS");
        assert_eq!(rs[1].data, b"-Wall -Wextra\n");
        // 実行時は code に書いた内容を bash で実行するだけ
        // code の内容は echo Hello; cat subfile なので、Hello と Subfile が表示される
        assert_eq!(rs[2].r#type, "StdOut");
        assert_eq!(rs[2].data, b"Hello\nSubfile");
        assert_eq!(rs[3].r#type, "ExitCode");
        assert_eq!(rs[3].data, b"0");
        assert_eq!(rs[4].r#type, "Control");
        assert_eq!(rs[4].data, b"Finish");

        let r: CompileResult = json.result.clone();
        assert_eq!(
            r,
            CompileResult {
                status: "0".to_string(),
                signal: "".to_string(),
                compiler_output: b"-Wall -Wextra\n".to_vec(),
                compiler_error: vec![],
                compiler_message: b"-Wall -Wextra\n".to_vec(),
                program_output: b"Hello\nSubfile".to_vec(),
                program_error: vec![],
                program_message: b"Hello\nSubfile".to_vec(),
                permlink: permlink.clone(),
                url: "https://wandbox.org/permlink/".to_string() + &permlink,
            }
        );
    }
}
