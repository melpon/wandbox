use crate::{
    config::convert_config_to_compiler_info,
    types::{AppConfig, CompilerInfo},
};
use axum::{Json, extract::State};
use std::sync::Arc;

pub async fn get_api_list_json(State(config): State<Arc<AppConfig>>) -> Json<Vec<CompilerInfo>> {
    let compiler_infos: Vec<CompilerInfo> =
        convert_config_to_compiler_info(&config.config, &config.version_info);
    Json(compiler_infos)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        config::get_version_info,
        create_app,
        types::{Config, PodmanConfig, Switch},
    };
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode},
    };
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_get_api_list_json() {
        let config: Config = serde_json::from_reader(
            std::fs::File::open("src/tests/data/api_list_input.json").unwrap(),
        )
        .unwrap();
        let podman_config: PodmanConfig = PodmanConfig::new("wandbox-runner");
        let app_config: AppConfig = AppConfig {
            podman: podman_config.clone(),
            config: config.clone(),
            version_info: get_version_info(&podman_config, &config).await.unwrap(),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let request: Request<Body> = Request::builder()
            .uri("/api/list.json")
            .body(Body::empty())
            .unwrap();

        // `oneshot()` を使ってリクエストをシミュレート
        let response: Response<Body> = app.oneshot(request).await.unwrap();

        // ステータスコードの確認
        assert_eq!(response.status(), StatusCode::OK);

        // レスポンスボディの確認
        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let json: Vec<CompilerInfo> = serde_json::from_slice(&body).unwrap();

        assert_eq!(
            json,
            vec![CompilerInfo {
                name: "test".to_string(),
                version: "0.1.2".to_string(),
                language: "C".to_string(),
                display_name: "test compiler".to_string(),
                templates: vec![],
                compiler_option_raw: true,
                runtime_option_raw: false,
                display_compile_command: "test prog.c".to_string(),
                switches: vec![Switch::Single {
                    name: "warning".to_string(),
                    display_name: "Warnings".to_string(),
                    display_flags: "-Wall -Wextra".to_string(),
                    default: true
                }]
            }]
        );
    }
}
