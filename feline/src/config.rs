use crate::podman::with_podman;
use crate::types::{CompilerInfo, Config, PodmanConfig, Switch, SwitchOption};
use std::collections::HashMap;

pub fn convert_config_to_compiler_info(
    config: &Config,
    version_info: &HashMap<String, String>,
) -> Vec<CompilerInfo> {
    config
        .compilers
        .iter()
        .map(|compiler| {
            let version: String = version_info.get(&compiler.name).unwrap().clone();

            let mut single_switches: Vec<Switch> = Vec::new();
            let mut select_groups: Vec<Switch> = Vec::new();

            for switch_name in &compiler.switches {
                if let Some(switch) = config.switches.get(switch_name) {
                    if let Some(group) = &switch.group {
                        let switch_option: SwitchOption = SwitchOption {
                            name: switch_name.clone(),
                            display_flags: if switch.display_flags.is_empty() {
                                switch.flags.join(" ")
                            } else {
                                switch.display_flags.clone()
                            },
                            display_name: switch.display_name.clone(),
                        };
                        let found: Option<&mut Switch> = select_groups
                            .iter_mut()
                            .find(|s| matches!(s, Switch::Select { name, .. } if name == group));
                        match found {
                            Some(Switch::Select { options, .. }) => {
                                options.push(switch_option);
                            }
                            Some(_) => unreachable!(),
                            None => {
                                select_groups.push(Switch::Select {
                                    name: group.clone(),
                                    options: vec![switch_option],
                                    default: "".to_string(),
                                });
                            }
                        }
                    } else {
                        single_switches.push(Switch::Single {
                            name: switch_name.clone(),
                            display_name: switch.display_name.clone(),
                            display_flags: switch.flags.join(" "),
                            default: compiler.initial_checked.contains(switch_name),
                        });
                    }
                }
            }

            let mut switches: Vec<Switch> = single_switches;
            for switch in select_groups {
                match switch {
                    Switch::Select { name, options, .. } => {
                        let match_option: Option<&SwitchOption> =
                            options.iter().find(|option: &&SwitchOption| {
                                return compiler.initial_checked.contains(&option.name);
                            });
                        let default: String = match match_option {
                            Some(option) => option.name.clone(),
                            None => options[0].name.clone(),
                        };
                        switches.push(Switch::Select {
                            name,
                            options,
                            default,
                        });
                    }
                    _ => unreachable!(),
                }
            }

            CompilerInfo {
                name: compiler.name.clone(),
                version,
                language: compiler.language.clone(),
                display_name: compiler.display_name.clone(),
                templates: compiler.templates.clone(),
                compiler_option_raw: compiler.compiler_option_raw,
                runtime_option_raw: compiler.runtime_option_raw,
                display_compile_command: compiler.display_compile_command.clone(),
                switches,
            }
        })
        .collect()
}

async fn run_version_command(
    config: &PodmanConfig,
    command: &[String],
) -> Result<String, anyhow::Error> {
    let stdout: Vec<u8> = with_podman(&config, &command[0], &command[1..].to_vec())
        .output()
        .await?
        .stdout;
    let str: String = String::from_utf8(stdout)?.trim().to_string();
    Ok(str)
}

pub async fn get_version_info(
    podman: &PodmanConfig,
    config: &Config,
) -> Result<HashMap<String, String>, anyhow::Error> {
    let mut version_info: HashMap<String, String> = HashMap::new();
    for compiler in &config.compilers {
        let version: String = run_version_command(&podman, &compiler.version_command).await?;
        version_info.insert(compiler.name.clone(), version);
    }
    Ok(version_info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Compiler;
    use std::fs;

    // 本番と同じ JSON データが読めるか確認する
    // scp wandbox-ubuntu-24.04:/opt/wandbox-data/release/cattleshed-conf/compilers.default ./
    // で拾ってきたデータを使ってテストする
    #[test]
    #[ignore]
    fn test_load() {
        let input_json: String =
            fs::read_to_string("./compilers.default").expect("Failed to read input JSON");
        let _config: Config =
            serde_json::from_str(&input_json).expect("Failed to parse input JSON");
    }

    #[test]
    fn test_convert_config() {
        let input_json: String = fs::read_to_string("src/tests/data/config_input.json")
            .expect("Failed to read input JSON");
        let expected_json: String = fs::read_to_string("src/tests/data/config_expected.json")
            .expect("Failed to read expected JSON");

        let config: Config = serde_json::from_str(&input_json).expect("Failed to parse input JSON");
        let expected_output: Vec<CompilerInfo> =
            serde_json::from_str(&expected_json).expect("Failed to parse expected JSON");

        // バージョンの計算は後でやる
        let version_info: HashMap<String, String> = HashMap::from([
            ("gcc-head-c".into(), "15.0.1 20250308 (experimental)".into()),
            ("gcc-13.2.0-c".into(), "13.2.0".into()),
        ]);
        let result: Vec<CompilerInfo> = convert_config_to_compiler_info(&config, &version_info);

        assert_eq!(result, expected_output);
    }

    #[tokio::test]
    async fn test_get_version_infos() {
        let config: Config = Config {
            switches: HashMap::new(),
            compilers: vec![Compiler {
                name: "gcc-head-c".to_string(),
                version_command: vec!["echo".to_string(), "0.1.2".to_string()],
                ..Compiler::default()
            }],
            templates: HashMap::new(),
        };
        let version_info: HashMap<String, String> =
            get_version_info(&PodmanConfig::new("wandbox-runner"), &config)
                .await
                .unwrap();
        assert_eq!(version_info.len(), 1);
        assert_eq!(version_info["gcc-head-c"], "0.1.2");
    }
}
