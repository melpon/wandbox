mod api_compile_ndjson;
mod api_list_json;
mod config;
mod db;
mod podman;
mod types;

use api_compile_ndjson::post_api_compile_ndjson;
use api_list_json::get_api_list_json;
use axum::{
    Router,
    routing::{get, post},
};
use clap::Parser;
use config::get_version_info;
use std::{fs, net::SocketAddr, sync::Arc};
use tokio::net::TcpListener;
use types::{AppConfig, Config, PodmanConfig};

#[derive(Parser, Debug)]
#[command(about = "Feline API server")]
struct Args {
    #[arg(long)]
    config_file: String,
    #[arg(long)]
    database_url: String,
    #[arg(long)]
    podman_config_file: String,
    #[arg(long)]
    safe_run_dir: String,
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
    let app: Router = create_app(AppConfig {
        database_url: args.database_url,
        config: config.clone(),
        podman: podman_config.clone(),
        version_info: get_version_info(&podman_config, &config).await.unwrap(),
        safe_run_dir: args.safe_run_dir,
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
        .with_state(shared_config);
}
