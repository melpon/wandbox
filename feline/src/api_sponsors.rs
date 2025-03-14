use crate::types::{AppConfig, SponsorResponse};
use axum::{Json, extract::State};
use std::sync::Arc;

pub async fn get_api_sponsors_json(State(_config): State<Arc<AppConfig>>) -> Json<SponsorResponse> {
    let sponsors: SponsorResponse = SponsorResponse {
        corporate: vec![],
        personal: vec![],
    };
    Json(sponsors)
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
    async fn test_get_api_sponsors_json() {
        let app_config: AppConfig = AppConfig {
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let request: Request<Body> = Request::builder()
            .uri("/api/sponsors.json")
            .body(Body::empty())
            .unwrap();

        let response: Response<Body> = app.oneshot(request).await.unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let json: SponsorResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(
            json,
            SponsorResponse {
                corporate: vec![],
                personal: vec![],
            }
        );
    }
}
