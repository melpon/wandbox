use serde::{Deserialize, Deserializer, Serialize, Serializer};
use sqlx::SqlitePool;
use std::{collections::HashMap, sync::Arc};

#[derive(Debug, Clone, Default)]
pub struct AppConfig {
    // Default で作りたいので Option にする
    // 通常は絶対値が入るので、unwrap しても問題ない
    pub sqlite: Option<Arc<SqlitePool>>,
    // https://wandbox.org/
    pub wandbox_url: String,
    pub podman: PodmanConfig,
    pub config: Config,
    pub version_info: HashMap<String, String>,
    pub safe_run_dir: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct PodmanConfig {
    pub image: String,
    // src, dst, writable
    pub mounts: Vec<(String, String, bool)>,
    pub workdir: String,
    // 利用可能な CPU 時間
    pub cpusec: u32,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct Config {
    pub switches: HashMap<String, ConfigSwitch>,
    pub compilers: Vec<Compiler>,
    pub templates: HashMap<String, Template>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct ConfigSwitch {
    pub flags: Vec<String>,
    #[serde(rename = "display-name")]
    pub display_name: String,
    #[serde(rename = "display-flags", default)]
    pub display_flags: String,
    #[serde(default)]
    pub runtime: bool,
    #[serde(default)]
    pub group: Option<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct Compiler {
    pub name: String,
    #[serde(rename = "compile-command")]
    pub compile_command: Vec<String>,
    #[serde(rename = "version-command")]
    pub version_command: Vec<String>,
    #[serde(rename = "display-name")]
    pub display_name: String,
    #[serde(rename = "display-compile-command")]
    pub display_compile_command: String,
    pub language: String,
    #[serde(rename = "output-file")]
    pub output_file: String,
    #[serde(rename = "run-command")]
    pub run_command: Vec<String>,
    pub displayable: bool,
    #[serde(rename = "compiler-option-raw", default)]
    pub compiler_option_raw: bool,
    #[serde(rename = "runtime-option-raw", default)]
    pub runtime_option_raw: bool,
    pub switches: Vec<String>,
    #[serde(rename = "initial-checked")]
    pub initial_checked: Vec<String>,
    #[serde(rename = "jail-name")]
    pub jail_name: String,
    pub templates: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Template {
    pub code: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct CompilerInfo {
    pub name: String,
    pub version: String,
    pub language: String,
    #[serde(rename = "display-name")]
    pub display_name: String,
    pub templates: Vec<String>,
    #[serde(rename = "compiler-option-raw")]
    pub compiler_option_raw: bool,
    #[serde(rename = "runtime-option-raw")]
    pub runtime_option_raw: bool,
    #[serde(rename = "display-compile-command")]
    pub display_compile_command: String,
    pub switches: Vec<Switch>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(tag = "type")]
pub enum Switch {
    #[serde(rename = "single")]
    Single {
        name: String,
        #[serde(rename = "display-name")]
        display_name: String,
        #[serde(rename = "display-flags")]
        display_flags: String,
        #[serde(rename = "default")]
        default: bool,
    },
    #[serde(rename = "select")]
    Select {
        name: String,
        options: Vec<SwitchOption>,
        #[serde(rename = "default")]
        default: String,
    },
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct SwitchOption {
    pub name: String,
    #[serde(rename = "display-flags")]
    pub display_flags: String,
    #[serde(rename = "display-name")]
    pub display_name: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Code {
    pub file: String,
    pub code: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct CompileNdjsonResult {
    #[serde(rename = "type")]
    pub r#type: String,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub data: Vec<u8>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct PostPermlinkRequest {
    pub title: String,
    pub description: String,
    pub compiler: String,
    pub code: String,
    pub options: String,
    pub compiler_option_raw: String,
    pub runtime_option_raw: String,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub stdin: Vec<u8>,
    pub github_user: String,
    pub codes: Vec<Code>,
    pub results: Vec<CompileNdjsonResult>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct PostPermlinkResponse {
    pub permlink: String,
    pub url: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct CompileParameter {
    pub compiler: String,
    pub code: String,
    pub codes: Vec<Code>,
    pub options: String,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub stdin: Vec<u8>,
    pub compiler_option_raw: String,
    pub runtime_option_raw: String,
    pub github_user: String,
    pub title: String,
    pub description: String,
    pub save: bool,
    pub created_at: i64,
    pub is_private: bool,
    pub compiler_info: CompilerInfo,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default)]
pub struct CompileResult {
    pub status: String,
    pub signal: String,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub compiler_output: Vec<u8>,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub compiler_error: Vec<u8>,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub compiler_message: Vec<u8>,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub program_output: Vec<u8>,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub program_error: Vec<u8>,
    #[serde(
        serialize_with = "serialize_utf8",
        deserialize_with = "deserialize_utf8"
    )]
    pub program_message: Vec<u8>,
    pub permlink: String,
    pub url: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct GetPermlinkResponse {
    pub parameter: CompileParameter,
    pub results: Vec<CompileNdjsonResult>,
    pub result: CompileResult,
}

// ✅ `Vec<u8>` を `String` に変換して JSON に格納
fn serialize_utf8<S>(bytes: &Vec<u8>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let utf8_string = String::from_utf8(bytes.clone()).map_err(serde::ser::Error::custom)?;
    serializer.serialize_str(&utf8_string)
}

// ✅ JSON `String` を `Vec<u8>` に戻す
fn deserialize_utf8<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    Ok(s.into_bytes()) // UTF-8 のバイナリとして `Vec<u8>` に変換
}
