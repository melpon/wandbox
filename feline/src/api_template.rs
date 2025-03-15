use crate::types::{AppConfig, AppError, Template, TemplateResponse};
use axum::{Json, extract::Path, extract::State};
use std::sync::Arc;

pub async fn get_api_template(
    State(config): State<Arc<AppConfig>>,
    Path(template_name): Path<String>,
) -> Result<Json<TemplateResponse>, AppError> {
    let t: &Template = config
        .config
        .templates
        .get(&template_name)
        .ok_or(anyhow::anyhow!("Template not found"))?;
    let r: TemplateResponse = TemplateResponse {
        name: template_name.clone(),
        code: t.code.clone(),
        codes: t.codes.clone(),
        stdin: t.stdin.clone(),
        options: t.options.clone(),
        compiler_option_raw: t.compiler_option_raw.clone(),
        runtime_option_raw: t.runtime_option_raw.clone(),
    };
    return Ok(Json(r));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{create_app, types::Config};
    use axum::{
        Router,
        body::{Body, Bytes},
        http::{Request, Response, StatusCode},
    };
    use http_body_util::BodyExt;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_get_api_template() {
        let config: Config = serde_json::from_reader(
            std::fs::File::open("src/tests/data/config_input.json").unwrap(),
        )
        .unwrap();
        let app_config: AppConfig = AppConfig {
            config: config.clone(),
            ..AppConfig::default()
        };
        let app: Router = create_app(app_config);

        let request: Request<Body> = Request::builder()
            .uri("/api/template/gcc-c")
            .body(Body::empty())
            .unwrap();

        let response: Response<Body> = app.oneshot(request).await.unwrap();

        assert_eq!(response.status(), StatusCode::OK);

        let body: Bytes = response.into_body().collect().await.unwrap().to_bytes();
        let json: TemplateResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(
            json,
            TemplateResponse {
                name: "gcc-c".to_string(),
                code: "#include <stdio.h>\n#include <stdlib.h>\n\nint main(void)\n{\n    puts(\"Hello, Wandbox!\");\n    return EXIT_SUCCESS;\n}\n".to_string(),
                ..TemplateResponse::default()
            }
        );
    }
}
