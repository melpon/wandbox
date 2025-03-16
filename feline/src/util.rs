use crate::types::{
    CompileNdjsonResult, CompileParameter, CompileResult, Issuer, PostPermlinkRequest,
};
use anyhow::Result;
use axum::http::HeaderMap;
use rand::{Rng as _, distr::Alphanumeric};

pub fn merge_compile_result(results: &Vec<CompileNdjsonResult>) -> CompileResult {
    // CompileResult を計算する
    let mut result: CompileResult = CompileResult::default();
    for r in results {
        match r.r#type.as_str() {
            "Control" => {}
            "CompilerMessageS" => {
                result.compiler_output.extend(&r.data);
                result.compiler_message.extend(&r.data);
            }
            "CompilerMessageE" => {
                result.compiler_error.extend(&r.data);
                result.compiler_message.extend(&r.data);
            }
            "StdOut" => {
                result.program_output.extend(&r.data);
                result.program_message.extend(&r.data);
            }
            "StdErr" => {
                result.program_error.extend(&r.data);
                result.program_message.extend(&r.data);
            }
            "ExitCode" => {
                result.status.push_str(&String::from_utf8_lossy(&r.data));
            }
            "Signal" => {
                result.signal.push_str(&String::from_utf8_lossy(&r.data));
            }
            _ => {
                // TODO: エラーハンドリング
            }
        }
    }
    result
}

pub fn make_random_str(n: usize) -> String {
    rand::rng()
        .sample_iter(&Alphanumeric)
        .take(n)
        .map(char::from)
        .collect::<String>()
}

pub fn make_permlink_request(
    kreq: &CompileParameter,
    results: &Vec<CompileNdjsonResult>,
) -> PostPermlinkRequest {
    PostPermlinkRequest {
        title: kreq.title.clone(),
        description: kreq.description.clone(),
        compiler: kreq.compiler.clone(),
        code: kreq.code.clone(),
        codes: kreq.codes.clone(),
        options: kreq.options.clone(),
        stdin: kreq.stdin.clone(),
        compiler_option_raw: kreq.compiler_option_raw.clone(),
        runtime_option_raw: kreq.runtime_option_raw.clone(),
        results: results.clone(),
        // わざと設定しない
        // github_user: kreq.github_user.clone(),
        github_user: "".to_string(),
    }
}

pub fn make_issuer(path: &str, headers: &HeaderMap, github_user: &str) -> Result<Issuer> {
    let mut issuer: Issuer = Issuer::default();
    if let Some(value) = headers.get("X-Real-IP") {
        issuer.real_ip = value.to_str()?.to_string();
    }
    if let Some(value) = headers.get("X-Forwarded-For") {
        issuer.forwarded_for = value.to_str()?.to_string();
    }
    issuer.path_info = path.to_string();
    issuer.github_username = github_user.to_string();
    Ok(issuer)
}

const BASE64_TABLE: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// 与えられたバイト列を Base64 エンコードする
pub fn base64_encode(input: &[u8]) -> String {
    let mut output: String = String::new();
    let mut buffer: u32 = 0;
    let mut bits: u8 = 0;

    for &byte in input {
        buffer = (buffer << 8) | byte as u32;
        bits += 8;

        while bits >= 6 {
            let index = (buffer >> (bits - 6)) & 0b111111;
            output.push(BASE64_TABLE[index as usize] as char);
            bits -= 6;
        }
    }

    // 余ったビットを埋める
    if bits > 0 {
        buffer <<= 6 - bits;
        output.push(BASE64_TABLE[(buffer & 0b111111) as usize] as char);
    }

    // パディングを追加
    while output.len() % 4 != 0 {
        output.push('=');
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_all_ascii() {
        let ascii: Vec<u8> = (0..=127).collect();

        assert_eq!(
            "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7P\
         D0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn8\
         =",
            &base64_encode(&ascii),
        );
    }

    #[test]
    fn encode_all_bytes() {
        let bytes: Vec<u8> = (0..=255).collect();

        assert_eq!(
            "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7P\
         D0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3eHl6e3x9fn\
         +AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKztLW2t7i5uru8vb6\
         /wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v8PHy8/T19vf4+fr7/P3+/w==",
            base64_encode(&bytes),
        );
    }
}
