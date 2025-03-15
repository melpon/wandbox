use crate::{
    types::{
        Code, CompileNdjsonResult, CompileParameter, CompileResult, CompilerInfo,
        GetPermlinkResponse, PostPermlinkRequest,
    },
    util::merge_compile_result,
};
use chrono::{NaiveDateTime, Utc};
use sqlx::{Error, Sqlite, SqlitePool, query, query_scalar};

pub async fn make_permlink(
    pool: &SqlitePool,
    permlink_name: &str,
    req: &PostPermlinkRequest,
    compiler_info: &CompilerInfo,
) -> Result<(), anyhow::Error> {
    let mut tx: sqlx::Transaction<'_, Sqlite> = pool.begin().await?;

    let now: NaiveDateTime = Utc::now().naive_local();
    let now_str: String = now.format("%Y-%m-%d %H:%M:%S").to_string();

    // code への追加
    let code_id: i64 = query_scalar!(
        "INSERT INTO code (title, description, compiler, code, optimize, warning, options, compiler_option_raw, runtime_option_raw, stdin, created_at, updated_at, github_user) 
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) 
         RETURNING id",
        req.title,
        req.description,
        req.compiler,
        req.code,
        false,
        false,
        req.options,
        req.compiler_option_raw,
        req.runtime_option_raw,
        req.stdin,
        now_str,
        now_str,
        req.github_user
    )
    .fetch_one(&mut *tx)
    .await?;

    // codes への追加
    if !req.codes.is_empty() {
        for (order, code) in req.codes.iter().enumerate() {
            let order_value: i32 = (order as i32) + 1;
            query!(
                "INSERT INTO codes (code_id, \"order\", file, code) VALUES (?, ?, ?, ?)",
                code_id,
                order_value,
                code.file,
                code.code
            )
            .execute(&mut *tx)
            .await?;
        }
    }

    // 保存時の compiler_info を保存
    // フィールド作ったりするのは面倒なので JSON で
    let compiler_info_json: String = serde_json::to_string(compiler_info)?;
    query!(
        "INSERT INTO compiler_info (code_id, json) VALUES (?, ?)",
        code_id,
        compiler_info_json
    )
    .execute(&mut *tx)
    .await?;

    // link への追加
    let link_id: i64 = query_scalar!(
        "INSERT INTO link (permlink, code_id) VALUES (?, ?) RETURNING id",
        permlink_name,
        code_id
    )
    .fetch_one(&mut *tx)
    .await?
    .ok_or(Error::RowNotFound)?;

    // link_output への追加
    for (order, output) in req.results.iter().enumerate() {
        let order_value: i32 = (order as i32) + 1;
        query!(
            "INSERT INTO link_output (link_id, \"order\", type, output) VALUES (?, ?, ?, ?)",
            link_id,
            order_value,
            output.r#type,
            output.data
        )
        .execute(&mut *tx)
        .await?;
    }

    tx.commit().await?;

    Ok(())
}

pub async fn get_permlink(
    pool: &SqlitePool,
    permlink_name: &str,
) -> Result<GetPermlinkResponse, anyhow::Error> {
    let mut tx: sqlx::Transaction<'_, Sqlite> = pool.begin().await?;

    // link テーブルから id と code_id を取得
    let (link_id, code_id) = sqlx::query_as::<_, (i64, i64)>(
        r#"SELECT id as "id!", code_id as "code_id!" FROM link WHERE permlink = ?"#,
    )
    .bind(permlink_name)
    .fetch_one(&mut *tx)
    .await?;

    // code_id から code テーブルの情報を取得
    let (
        compiler,
        code,
        options,
        compiler_option_raw,
        runtime_option_raw,
        stdin,
        created_at,
        title,
        description,
        github_user,
        private,
    ) = sqlx::query_as::<
        _,
        (
            String,
            String,
            String,
            String,
            String,
            Vec<u8>,
            String,
            String,
            String,
            String,
            bool,
        ),
    >(
        r#"
            SELECT
              compiler,
              code,
              options as "options!",
              compiler_option_raw as "compiler_option_raw!",
              runtime_option_raw as "runtime_option_raw!",
              stdin as "stdin!",
              created_at as "created_at!",
              title as "title!",
              description as "description!",
              github_user as "github_user!",
              private as "private!"
           FROM code WHERE id = ?
        "#,
    )
    .bind(code_id)
    .fetch_one(&mut *tx)
    .await?;

    let mut parameter: CompileParameter = CompileParameter {
        compiler,
        code,
        options,
        stdin,
        compiler_option_raw,
        runtime_option_raw,
        github_user,
        title,
        description,
        save: false,
        created_at: NaiveDateTime::parse_from_str(&created_at, "%Y-%m-%d %H:%M:%S")
            .unwrap()
            .and_utc()
            .timestamp(),
        is_private: private,
        compiler_info: CompilerInfo::default(),
        codes: vec![],
    };

    // code_id から codes テーブルの情報を取得
    let codes: Vec<Code> = sqlx::query_as!(
        Code,
        r#"SELECT file as "file!", code as "code!"FROM codes WHERE code_id = ? ORDER BY "order""#,
        code_id
    )
    .fetch_all(&mut *tx)
    .await?;
    parameter.codes = codes;

    // link_id から link_output テーブルの情報を取得
    let results: Vec<CompileNdjsonResult> = sqlx::query_as!(
        CompileNdjsonResult,
        "SELECT type, output as data FROM link_output WHERE link_id = ? ORDER BY \"order\"",
        link_id
    )
    .fetch_all(&mut *tx)
    .await?;

    // code_id から compiler_info テーブルの情報を取得（存在する場合）
    if let Some(row) = sqlx::query!("SELECT json FROM compiler_info WHERE code_id = ?", code_id)
        .fetch_optional(&mut *tx)
        .await?
    {
        parameter.compiler_info = serde_json::from_str(&row.json).unwrap_or_default();
    }

    tx.commit().await?;

    // CompileResult を計算する
    let mut result: CompileResult = merge_compile_result(&results);
    result.permlink = permlink_name.to_string();
    result.url = format!("https://wandbox.org/permlink/{}", permlink_name);

    Ok(GetPermlinkResponse {
        parameter,
        results,
        result,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{
        Code, CompileNdjsonResult, CompilerInfo, PostPermlinkRequest, Switch, SwitchOption,
    };
    use sqlx::{SqlitePool, migrate::Migrator};
    use std::path::Path;

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

    fn fixture_post_permlink_request() -> PostPermlinkRequest {
        PostPermlinkRequest {
            title: "Test Title".to_string(),
            description: "Test Description".to_string(),
            compiler: "gcc".to_string(),
            code: "#include <stdio.h>\nint main() { return 0; }".to_string(),
            options: "".to_string(),
            compiler_option_raw: "".to_string(),
            runtime_option_raw: "".to_string(),
            stdin: b"".to_vec(),
            github_user: "test_user".to_string(),
            codes: vec![Code {
                file: "file1.c".to_string(),
                code: "#include <stdio.h>".to_string(),
            }],
            results: vec![
                CompileNdjsonResult {
                    r#type: "StdOut".to_string(),
                    data: b"Hello, World!\n".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "StdErr".to_string(),
                    data: b"hoge\n".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "StdErr".to_string(),
                    data: b"fuga\n".to_vec(),
                },
                CompileNdjsonResult {
                    r#type: "ExitCode".to_string(),
                    data: b"0".to_vec(),
                },
            ],
        }
    }

    fn fixture_compiler_info() -> CompilerInfo {
        CompilerInfo {
            name: "gcc-13.2.0".to_string(),
            version: "13.2.0".to_string(),
            language: "C".to_string(),
            display_name: "GCC 13.2.0".to_string(),
            templates: vec!["gcc-c".to_string()],
            compiler_option_raw: true,
            runtime_option_raw: false,
            display_compile_command: "gcc main.c -o main".to_string(),
            switches: vec![
                Switch::Single {
                    name: "warnings".to_string(),
                    display_name: "Enable Warnings".to_string(),
                    display_flags: "-Wall -Wextra".to_string(),
                    default: true,
                },
                Switch::Select {
                    name: "std-c".to_string(),
                    options: vec![
                        SwitchOption {
                            name: "c11".to_string(),
                            display_flags: "-std=c11".to_string(),
                            display_name: "C11".to_string(),
                        },
                        SwitchOption {
                            name: "gnu11".to_string(),
                            display_flags: "-std=gnu11".to_string(),
                            display_name: "GNU C11".to_string(),
                        },
                    ],
                    default: "c11".to_string(),
                },
            ],
        }
    }

    #[tokio::test]
    async fn test_make_permlink() {
        let pool: SqlitePool = setup_test_db().await;

        let permlink_name: String = "test_permlink".to_string();
        let req: PostPermlinkRequest = fixture_post_permlink_request();
        let compiler_info: CompilerInfo = fixture_compiler_info();

        let result: Result<(), anyhow::Error> =
            make_permlink(&pool, &permlink_name, &req, &compiler_info).await;
        assert!(result.is_ok());

        let code_count: i64 = sqlx::query_scalar!("SELECT COUNT(*) FROM code")
            .fetch_one(&pool)
            .await
            .unwrap();
        assert_eq!(code_count, 1);

        let codes_count: i64 = sqlx::query_scalar!("SELECT COUNT(*) FROM codes")
            .fetch_one(&pool)
            .await
            .unwrap();
        assert_eq!(codes_count, req.codes.len() as i64);

        let compiler_info_count: i64 = sqlx::query_scalar!("SELECT COUNT(*) FROM compiler_info")
            .fetch_one(&pool)
            .await
            .unwrap();
        assert_eq!(compiler_info_count, 1);

        let link_count: i64 = sqlx::query_scalar!("SELECT COUNT(*) FROM link")
            .fetch_one(&pool)
            .await
            .unwrap();
        assert_eq!(link_count, 1);

        let link_output_count: i64 = sqlx::query_scalar!("SELECT COUNT(*) FROM link_output")
            .fetch_one(&pool)
            .await
            .unwrap();
        assert_eq!(link_output_count, req.results.len() as i64);
    }

    #[tokio::test]
    async fn test_get_permlink() {
        let pool: SqlitePool = setup_test_db().await;

        let permlink_name: String = "test_permlink".to_string();
        let req: PostPermlinkRequest = fixture_post_permlink_request();
        let compiler_info: CompilerInfo = fixture_compiler_info();

        let result: Result<(), anyhow::Error> =
            make_permlink(&pool, &permlink_name, &req, &compiler_info).await;
        assert!(result.is_ok());

        let response: Result<GetPermlinkResponse, anyhow::Error> =
            get_permlink(&pool, &permlink_name).await;
        assert!(response.is_ok(), "get_permlink failed: {:?}", response);

        let response: GetPermlinkResponse = response.unwrap();

        // permlink が正しいか確認
        assert_eq!(response.result.permlink, permlink_name);
        assert_eq!(
            response.result.url,
            format!("https://wandbox.org/permlink/{}", permlink_name)
        );

        // CompileParameter の各フィールドを検証
        assert_eq!(response.parameter.compiler, req.compiler);
        assert_eq!(response.parameter.code, req.code);
        assert_eq!(response.parameter.options, req.options);
        assert_eq!(response.parameter.stdin, req.stdin);
        assert_eq!(
            response.parameter.compiler_option_raw,
            req.compiler_option_raw
        );
        assert_eq!(
            response.parameter.runtime_option_raw,
            req.runtime_option_raw
        );
        assert_eq!(response.parameter.github_user, req.github_user);
        assert_eq!(response.parameter.title, req.title);
        assert_eq!(response.parameter.description, req.description);
        assert_eq!(response.parameter.is_private, false);

        // codes フィールドの比較
        assert_eq!(response.parameter.codes, req.codes);

        // compiler_info の比較
        assert_eq!(response.parameter.compiler_info, compiler_info);

        // results フィールドの比較
        assert_eq!(response.results, req.results);

        // result フィールドの比較
        assert_eq!(response.result.status, "0");
        assert_eq!(response.result.compiler_output, b"");
        assert_eq!(response.result.compiler_error, b"");
        assert_eq!(response.result.compiler_message, b"");
        assert_eq!(response.result.program_output, b"Hello, World!\n");
        assert_eq!(response.result.program_error, b"hoge\nfuga\n");
        assert_eq!(
            response.result.program_message,
            b"Hello, World!\nhoge\nfuga\n"
        );
    }
}
