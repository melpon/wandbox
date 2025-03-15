mod api_compile_json;
mod api_compile_ndjson;
mod api_hpplib;
mod api_list_json;
mod api_permlink;
mod api_sponsors;
mod api_template;
mod config;
mod db;
mod podman;
mod types;
mod util;

use api_compile_json::post_api_compile_json;
use api_compile_ndjson::post_api_compile_ndjson;
use api_hpplib::get_api_hpplib;
use api_list_json::get_api_list_json;
use api_permlink::get_api_permlink;
use api_permlink::post_api_permlink;
use api_sponsors::get_api_sponsors_json;
use api_template::get_api_template;
use axum::{
    Router,
    body::{Body, Bytes, to_bytes},
    extract,
    http::{Method, Response, StatusCode, Uri},
    middleware::{Next, from_fn},
    response,
    routing::{get, post},
};
use clap::Parser;
use config::get_version_info;
use sqlx::SqlitePool;
use sqlx::migrate::Migrator;
use std::path::Path;
use std::{fs, net::SocketAddrV4, sync::Arc};
use tokio::net::TcpListener;
use types::{AppConfig, Config, PodmanConfig};

#[derive(Parser, Debug)]
#[command(about = "Feline API server")]
struct Args {
    #[arg(long)]
    bind: String,
    #[arg(long)]
    config_file: String,
    #[arg(long)]
    hpplib_file: String,
    #[arg(long)]
    database_url: String,
    #[arg(long)]
    database_migration_dir: String,
    #[arg(long)]
    podman_config_file: String,
    #[arg(long)]
    safe_run_dir: String,
    #[arg(long)]
    safe_run_log_dir: String,
    #[arg(long)]
    wandbox_url: String,
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let args: Args = Args::parse();
    let config: Config =
        serde_json::from_str(fs::read_to_string(args.config_file).unwrap().as_str()).unwrap();
    let podman_config: PodmanConfig = serde_json::from_str(
        fs::read_to_string(args.podman_config_file.clone())
            .unwrap()
            .as_str(),
    )
    .unwrap();

    let sqlite: Arc<SqlitePool> = Arc::new(SqlitePool::connect(&args.database_url).await.unwrap());
    let migrator: Migrator = Migrator::new(Path::new(&args.database_migration_dir))
        .await
        .unwrap();
    migrator.run(&*sqlite).await.unwrap();

    let app: Router = create_app(AppConfig {
        sqlite: Some(sqlite),
        wandbox_url: args.wandbox_url,
        config: config.clone(),
        podman: podman_config.clone(),
        version_info: get_version_info(&podman_config, &config).await.unwrap(),
        safe_run_dir: args.safe_run_dir,
        safe_run_log_dir: args.safe_run_log_dir,
        hpplib_file: args.hpplib_file.into(),
    });

    let addr: SocketAddrV4 = args.bind.parse().unwrap();
    log::info!("Server running on http://{}", addr);
    let listener: TcpListener = tokio::net::TcpListener::bind(addr).await.unwrap();

    axum::serve(listener, app).await.unwrap();
}

async fn log_request_and_response(req: extract::Request, next: Next) -> response::Response {
    let method: Method = req.method().clone();
    let uri: Uri = req.uri().clone();

    let resp: Response<Body> = next.run(req).await;

    let status: StatusCode = resp.status().clone();
    if status.is_client_error() || status.is_server_error() {
        let (parts, body) = resp.into_parts();
        let bytes: Bytes = to_bytes(body, 10240)
            .await
            .unwrap_or("Failed to read body".into());

        log::warn!(
            "[{}] {} {}: {}",
            status,
            method,
            uri,
            String::from_utf8_lossy(&bytes),
        );

        // 再度レスポンスを組み立てて返す
        return Response::from_parts(parts, Body::from(bytes));
    } else {
        log::info!("[{}] {} {}", resp.status(), method, uri);
    }

    resp
}

fn create_app(config: AppConfig) -> Router {
    let shared_config: Arc<AppConfig> = Arc::new(config);
    return Router::new()
        .route("/api/list.json", get(get_api_list_json))
        .route("/api/compile.ndjson", post(post_api_compile_ndjson))
        .route("/api/compile.json", post(post_api_compile_json))
        .route("/api/permlink/{permlink_id}", get(get_api_permlink))
        .route("/api/permlink", post(post_api_permlink))
        .route("/api/sponsors.json", get(get_api_sponsors_json))
        .route("/api/template/{template_name}", get(get_api_template))
        .route("/api/hpplib.json", get(get_api_hpplib))
        .layer(from_fn(log_request_and_response))
        .with_state(shared_config);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::AppError;
    use axum::http::Request;
    use http_body_util::BodyExt as _;
    use tower::ServiceExt as _;

    async fn ok_handler() -> Result<String, AppError> {
        Ok("ok".to_string())
    }
    async fn error_handler() -> Result<String, AppError> {
        Err(anyhow::anyhow!("error"))?
    }

    #[tokio::test]
    async fn test_logging_middleware() {
        let _ = env_logger::builder().is_test(true).try_init();

        let app: Router = Router::new()
            .route("/ok", get(ok_handler))
            .route("/error", get(error_handler))
            .layer(from_fn(log_request_and_response));

        let request: Request<Body> = axum::extract::Request::builder()
            .uri("/ok")
            .body(Body::empty())
            .unwrap();
        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::OK);
        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let body_text: String = String::from_utf8(body.to_vec()).unwrap();
        assert_eq!(body_text, "ok");

        let request: Request<Body> = axum::extract::Request::builder()
            .uri("/error")
            .body(Body::empty())
            .unwrap();
        let response: Response<Body> = app.clone().oneshot(request).await.unwrap();
        assert_eq!(response.status(), StatusCode::INTERNAL_SERVER_ERROR);
        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let body_text: String = String::from_utf8(body.to_vec()).unwrap();
        assert_eq!(body_text, "Error: error");

        // 出力をキャプチャするのが面倒なのでテストはしないが、以下のコマンドを実行すると：
        //
        // RUST_LOG=info cargo test tests::test_logging_middleware -- --nocapture
        //
        // 以下の様にログが出力される：
        //
        // [2025-03-14T20:44:50Z INFO  feline] [200 OK] GET /ok
        // [2025-03-14T20:44:50Z WARN  feline] [500 Internal Server Error] GET /error: Error: error
    }
}
