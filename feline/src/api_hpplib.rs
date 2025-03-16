use crate::types::{AppConfig, AppError};
use axum::{
    extract::State,
    http::StatusCode,
    http::header::CONTENT_TYPE,
    response::{IntoResponse, Response},
};
use std::sync::Arc;
use tokio::fs;

pub async fn get_api_hpplib(
    State(config): State<Arc<AppConfig>>,
) -> Result<impl IntoResponse, AppError> {
    let str: String = fs::read_to_string(&config.hpplib_file).await?;
    let resp: Response<String> = Response::builder()
        .status(StatusCode::OK)
        .header(CONTENT_TYPE, "application/json")
        .body(str)?;
    Ok(resp)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::create_app;
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode},
    };
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_get_api_template() {
        let app_config: AppConfig = AppConfig {
            hpplib_file: "src/tests/data/hpplib.json".into(),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let request: Request<Body> = Request::builder()
            .uri("/api/hpplib.json")
            .body(Body::empty())
            .unwrap();

        let response: Response<Body> = app.oneshot(request).await.unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let str = String::from_utf8(body.to_vec()).unwrap();

        assert_eq!(
            str,
            fs::read_to_string("src/tests/data/hpplib.json")
                .await
                .unwrap()
        );
    }
}
