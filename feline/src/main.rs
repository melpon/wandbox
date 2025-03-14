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
    routing::{get, post},
};
use clap::Parser;
use config::get_version_info;
use sqlx::SqlitePool;
use std::{fs, net::SocketAddr, sync::Arc};
use tokio::net::TcpListener;
use types::{AppConfig, Config, PodmanConfig};

#[derive(Parser, Debug)]
#[command(about = "Feline API server")]
struct Args {
    #[arg(long)]
    config_file: String,
    #[arg(long)]
    hpplib_file: String,
    #[arg(long)]
    database_url: String,
    #[arg(long)]
    podman_config_file: String,
    #[arg(long)]
    safe_run_dir: String,
    #[arg(long)]
    wandbox_url: String,
}

#[tokio::main]
async fn main() {
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
    let app: Router = create_app(AppConfig {
        sqlite: Some(sqlite),
        wandbox_url: args.wandbox_url,
        config: config.clone(),
        podman: podman_config.clone(),
        version_info: get_version_info(&podman_config, &config).await.unwrap(),
        safe_run_dir: args.safe_run_dir,
        hpplib_file: args.hpplib_file.into(),
    });

    // バインドするアドレス
    let addr: SocketAddr = SocketAddr::from(([127, 0, 0, 1], 3000));
    println!("Server running on http://{}", addr);
    let listener: TcpListener = tokio::net::TcpListener::bind(addr).await.unwrap();

    // サーバーの起動
    axum::serve(listener, app).await.unwrap();
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
        .with_state(shared_config);
}
