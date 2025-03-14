use crate::types::{CompileNdjsonResult, CompileParameter, CompileResult, PostPermlinkRequest};
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
