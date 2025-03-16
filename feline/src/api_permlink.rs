use crate::{
    config::convert_config_to_compiler_info,
    db::{get_permlink, make_permlink},
    types::{
        AppConfig, AppError, CompilerInfo, GetPermlinkResponse, PostPermlinkRequest,
        PostPermlinkResponse,
    },
    util::make_random_str,
};
use axum::{Json, extract::Path, extract::State};
use std::sync::Arc;

pub async fn get_api_permlink(
    State(config): State<Arc<AppConfig>>,
    Path(permlink_id): Path<String>,
) -> Result<Json<GetPermlinkResponse>, AppError> {
    let permlink: GetPermlinkResponse = get_permlink(
        &config.sqlite.clone().unwrap(),
        &permlink_id,
        &config.wandbox_url,
    )
    .await?;
    return Ok(Json(permlink));
}

pub async fn post_api_permlink(
    State(config): State<Arc<AppConfig>>,
    Json(body): Json<PostPermlinkRequest>,
) -> Result<Json<PostPermlinkResponse>, AppError> {
    let compiler_infos: Vec<CompilerInfo> =
        convert_config_to_compiler_info(&config.config, &config.version_info);
    let compiler_info: &CompilerInfo = compiler_infos
        .iter()
        .find(|c| c.name == body.compiler)
        .ok_or(anyhow::anyhow!("Unknown compiler"))?;

    let permlink_name: String = make_random_str(16);
    make_permlink(
        &config.sqlite.clone().unwrap(),
        &permlink_name,
        &body,
        &compiler_info,
    )
    .await?;
    let response = PostPermlinkResponse {
        permlink: permlink_name.clone(),
        url: format!("{}permlink/{}", config.wandbox_url, permlink_name),
    };

    return Ok(Json(response));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AppConfig, create_app,
        types::{
            Code, CompileNdjsonResult, CompileParameter, CompileResult, CompilerInfo, Config,
            Switch,
        },
    };
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode, header},
    };
    use http_body_util::BodyExt;
    use sqlx::{SqlitePool, migrate::Migrator};
    use std::collections::HashMap;
    use std::path::Path;
    use tower::ServiceExt;

    async fn setup_test_db() -> SqlitePool {
        let pool: SqlitePool = SqlitePool::connect(":memory:")
            .await
            .expect("Failed to create in-memory DB");

        // マイグレーション
        let migrator: Migrator = Migrator::new(Path::new("./migrations"))
            .await
            .expect("Failed to load migrations");
        migrator
            .run(&pool)
            .await
            .expect("Failed to apply migrations");

        pool
    }

    #[tokio::test]
    async fn test_api_permlink() {
        let sqlite: SqlitePool = setup_test_db().await;
        let config: Config = serde_json::from_reader(
            std::fs::File::open("src/tests/data/api_compile_input.json").unwrap(),
        )
        .unwrap();
        let app_config: AppConfig = AppConfig {
            sqlite: Some(Arc::new(sqlite)),
            config: config.clone(),
            wandbox_url: "https://wandbox.org/".to_string(),
            version_info: HashMap::from([("test".to_string(), "0.1.2".to_string())]),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let pbody: PostPermlinkRequest = PostPermlinkRequest {
            title: "title".to_string(),
            description: "description".to_string(),
            compiler: "test".to_string(),
            code: "code".to_string(),
            options: "options".to_string(),
            compiler_option_raw: "compiler_option_raw".to_string(),
            runtime_option_raw: "runtime_option_raw".to_string(),
            stdin: b"stdin".to_vec(),
            github_user: "github_user".to_string(),
            codes: vec![Code {
                file: "subfile".to_string(),
                code: "subcode".to_string(),
            }],
            results: vec![
                CompileNdjsonResult {
                    r#type: "Control".to_string(),
                    data: b"Start".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "CompilerMessageS".to_string(),
                    data: b"-Wall -Wextra\n".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "StdOut".to_string(),
                    data: b"Hello\nSubfile".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "ExitCode".to_string(),
                    data: b"0".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "Control".to_string(),
                    data: b"Finish".to_vec(),
                },
            ],
        };
        let request: Request<Body> = Request::builder()
            .method("POST")
            .uri("/api/permlink")
            .header(header::CONTENT_TYPE, "application/json")
            .body(Body::from(serde_json::to_string(&pbody).unwrap()))
            .unwrap();

        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        assert!(!body.is_empty());
        let json: PostPermlinkResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(json.permlink.len(), 16);
        assert_eq!(
            json.url,
            "https://wandbox.org/permlink/".to_string() + &json.permlink
        );

        let permlink: String = json.permlink.clone();

        // パーマリンクの取得
        let request: Request<Body> = Request::builder()
            .uri("/api/permlink/".to_string() + &json.permlink)
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
                title: pbody.title.clone(),
                description: pbody.description.clone(),
                compiler: pbody.compiler.clone(),
                code: pbody.code.clone(),
                options: pbody.options.clone(),
                compiler_option_raw: pbody.compiler_option_raw.clone(),
                runtime_option_raw: pbody.runtime_option_raw.clone(),
                stdin: pbody.stdin.clone(),
                github_user: pbody.github_user.clone(),
                codes: pbody.codes.clone(),
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

        assert_eq!(json.results, pbody.results);

        assert_eq!(
            json.result,
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
