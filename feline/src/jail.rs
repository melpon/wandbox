use std::{
    path::{Path, PathBuf},
    process::Output,
    sync::Arc,
};

use crate::{
    types::{
        AppConfig, Code, CompileNdjsonResult, CompileParameter, Compiler, Config, Issuer,
        PodmanConfig, RunLog,
    },
    util::base64_encode,
};
use tokio::{
    fs::{File, create_dir_all},
    io::{AsyncReadExt, AsyncWriteExt},
    process::{Child, ChildStderr, ChildStdin, ChildStdout, Command},
    sync::mpsc,
    time::{Duration, sleep},
};

impl PodmanConfig {
    #[cfg(test)]
    pub fn new(image: &str) -> Self {
        Self {
            image: image.to_string(),
            ..PodmanConfig::default()
        }
    }
    // ユーザーの作業用ディレクトリ
    pub fn with_workdir(self: &Self, src: &str, dst: &str) -> Self {
        let mut mounts: Vec<(String, String, bool)> = self.mounts.clone();
        mounts.push((src.to_string(), dst.to_string(), true));
        return Self {
            mounts: mounts,
            workdir: dst.to_string(),
            ..self.clone()
        };
    }
}

pub fn with_podman(config: &PodmanConfig, program: &str, args: &[String]) -> Command {
    // $ nice -- \
    //     podman run --rm --init -i \
    //     -v src:dst:ro -v src2:dst2 --workdir dst2 \
    //     --ulimit cpu=30 --ulimit nofile=1024 \
    //     --ulimit ... \
    //     $image $program $args
    let podman_program: &str = "nice";
    let podman_args: [&str; 3] = ["--", "podman", "run"];

    let mut xs: Vec<String> = vec![];
    // podman run に渡す引数
    xs.extend(["--rm", "--init", "-i"].map(|x| x.to_string()));
    for (src, dst, writable) in &config.mounts {
        let ro: &str = if *writable { "" } else { ":ro" };
        xs.push("-v".to_string());
        xs.push(format!("{}:{}{}", src, dst, ro));
    }
    if config.workdir != "" {
        xs.push("--workdir".to_string());
        xs.push(config.workdir.clone());
    }

    // ulimit シリーズ
    let ulimits = [
        Some(if config.cpu > 0 { config.cpu } else { 30 }),
        config.nofile,
        config.data,
        config.fsize,
        config.nproc,
    ];
    let ulimit_names = ["cpu", "nofile", "data", "fsize", "nproc"];
    for (ulimit, name) in ulimits.iter().zip(ulimit_names.iter()) {
        if let Some(ulimit) = ulimit {
            xs.extend(["--ulimit".to_string(), format!("{}={}", name, ulimit)]);
        }
    }

    if log::log_enabled!(log::Level::Info) {
        let args: String = [podman_program.to_string()]
            .iter()
            .chain(&podman_args.map(|x| x.to_string()))
            .chain(xs.iter())
            .chain([config.image.clone(), program.to_string()].iter())
            .chain(args.iter())
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        log::info!("Command: {}", args);
    }
    let mut cmd: Command = Command::new(podman_program);
    cmd.args(&podman_args)
        .args(&xs)
        .arg(&config.image)
        .arg(&program)
        .args(args);
    return cmd;
}

pub fn run_streaming(
    config: &PodmanConfig,
    program: &str,
    args: Vec<String>,
    stdin_data: Vec<u8>,
    sigkill_timeout: Duration,
) -> mpsc::Receiver<CompileNdjsonResult> {
    let config: PodmanConfig = config.clone();
    let program: String = program.to_string();

    let (tx, rx) = mpsc::channel::<CompileNdjsonResult>(100);

    tokio::spawn(async move {
        let txout: mpsc::Sender<CompileNdjsonResult> = tx.clone();
        let txerr: mpsc::Sender<CompileNdjsonResult> = tx.clone();
        let txexit: mpsc::Sender<CompileNdjsonResult> = tx.clone();
        let r: Result<(), anyhow::Error> = async move {
            let mut command: Command = with_podman(&config, &program, &args);
            let mut child: Child = command
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn()?;

            let mut stdin: ChildStdin = child
                .stdin
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stdin"))?;
            let mut stdout: ChildStdout = child
                .stdout
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stdout"))?;
            let mut stderr: ChildStderr = child
                .stderr
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stderr"))?;

            tokio::spawn(async move {
                let mut n = 0;
                while n < stdin_data.len() {
                    let end = std::cmp::min(n + 1024, stdin_data.len());
                    let r: Result<(), std::io::Error> = stdin.write_all(&stdin_data[n..end]).await;
                    if let Err(e) = r {
                        log::error!("Error: {}", e);
                        return;
                    }
                    let r = stdin.flush().await;
                    if let Err(e) = r {
                        log::error!("Error: {}", e);
                        return;
                    }
                    n = end;
                }
                // stdin を閉じる
                drop(stdin);
            });

            tokio::spawn(async move {
                let mut buf = [0 as u8; 1024];
                while let Ok(n) = stdout.read(&mut buf).await {
                    if n == 0 {
                        break;
                    }
                    let r = txout
                        .send(CompileNdjsonResult {
                            r#type: "StdOut".to_string(),
                            data: buf[..n].to_vec(),
                        })
                        .await;
                    if let Err(e) = r {
                        log::error!("Error: {}", e);
                    }
                }
            });

            tokio::spawn(async move {
                let mut buf = [0 as u8; 1024];
                while let Ok(n) = stderr.read(&mut buf).await {
                    if n == 0 {
                        break;
                    }
                    let r = txerr
                        .send(CompileNdjsonResult {
                            r#type: "StdErr".to_string(),
                            data: buf[..n].to_vec(),
                        })
                        .await;
                    if let Err(e) = r {
                        log::error!("Error: {}", e);
                    }
                }
            });

            tokio::select! {
                status = child.wait() => {
                    if let Err(e) = status {
                        return Err(e.into());
                    }
                    let status = status.unwrap();
                    #[cfg(unix)]
                    {
                        use std::os::unix::process::ExitStatusExt;
                        if let Some(signal) = status.signal() {
                            txexit
                                .send(CompileNdjsonResult {
                                    r#type: "Signal".to_string(),
                                    data: signal.to_string().as_bytes().to_vec(),
                                })
                                .await?;
                        }
                    }

                    if let Some(code) = status.code() {
                        txexit
                            .send(CompileNdjsonResult {
                                r#type: "ExitCode".to_string(),
                                data: code.to_string().as_bytes().to_vec(),
                            })
                            .await?;
                    }
                }
                _ = sleep(sigkill_timeout) => {
                    child.kill().await?;
                    txexit
                        .send(CompileNdjsonResult {
                            r#type: "Control".to_string(),
                            data: b"Timeout".to_vec(),
                        })
                        .await?;
                }
            }
            Ok(())
        }
        .await;
        if let Err(e) = r {
            let r = tx
                .send(CompileNdjsonResult {
                    r#type: "SystemError".to_string(),
                    data: e.to_string().as_bytes().to_vec(),
                })
                .await;
            if let Err(e) = r {
                log::error!("Error: {}", e);
            }
        }
    });

    return rx;
}

async fn create_file_with_dirs(path: &Path, content: &[u8]) -> Result<(), anyhow::Error> {
    if let Some(parent) = path.parent() {
        create_dir_all(parent).await?;
    }
    let mut file: File = File::create(&path).await?;
    file.write_all(&content).await?;
    Ok(())
}

// 戻り値のパスが必ず dir の下にあることを保証する
// file_path が絶対パスだったり "../" みたいな文字列が含まれて dir の外を指定したらエラーになる
pub fn resolve_safe_path(dir: &Path, file_path: &Path) -> Result<PathBuf, anyhow::Error> {
    // 絶対パスは一括でアウト
    if file_path.is_absolute() {
        return Err(anyhow::anyhow!("Invalid path"));
    }
    // dir が空文字なのは多分設定を忘れてるのでエラーにしておく
    if dir
        .to_str()
        .ok_or(anyhow::anyhow!("Invalid dir"))?
        .is_empty()
    {
        return Err(anyhow::anyhow!("dir is empty"));
    }

    let full_path = dir.join(file_path);
    let mut full_path_normed: PathBuf = PathBuf::new();

    let mut components = full_path.components();
    while let Some(component) = components.next() {
        match component {
            std::path::Component::Normal(part) => {
                full_path_normed.push(part);
            }
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                if !full_path_normed.pop() {
                    return Err(anyhow::anyhow!("Invalid path"));
                }
            }
            std::path::Component::RootDir => {
                full_path_normed.push("/");
            }
            _ => {
                return Err(anyhow::anyhow!("Invalid path"));
            }
        }
    }
    if full_path_normed.starts_with(dir) {
        return Ok(full_path_normed);
    } else {
        return Err(anyhow::anyhow!("Invalid path"));
    }
}

pub fn resolve_arguments(
    config: &Config,
    options: &str,
    option_raw: &str,
    runtime: bool,
) -> Vec<String> {
    let mut args: Vec<String> = vec![];
    // options は , 区切りのオプション
    for opt in options.split(',') {
        // switches に一致するオプションがあったら flags を追加
        if let Some(switches) = config.switches.get(opt) {
            if switches.runtime != runtime {
                continue;
            }
            args.extend(switches.flags.clone());
        }
    }
    // option_raw は \n 区切りのオプション
    let mut opts: Vec<&str> = option_raw.split('\n').collect();
    if !opts.is_empty() && opts[opts.len() - 1].is_empty() {
        opts.pop();
    }
    args.extend(opts.iter().map(|v| v.to_string()));
    args
}

fn get_compiler<'a>(config: &'a Config, name: &str) -> Result<&'a Compiler, anyhow::Error> {
    config
        .compilers
        .iter()
        .find(|v| &v.name == name)
        .ok_or(anyhow::anyhow!("Compiler not found"))
}

pub async fn prepare_environment(
    config: &Config,
    safe_run_dir: &str,
    timestamp: &chrono::DateTime<chrono::Local>,
    unique_name: &str,
    compiler: &str,
    code: &str,
    codes: &Vec<Code>,
    image: &str,
) -> Result<PathBuf, anyhow::Error> {
    let info: &Compiler = get_compiler(&config, &compiler)?;

    // ユーザーが実行するための環境を作る
    // ここは podman に守られてないので注意して実装する
    let base_dir: PathBuf = {
        let fmtstr: String = timestamp.format("%Y%m%d_%H%M%S").to_string();
        let dir: String = format!("wandbox_{}_{}", fmtstr, unique_name);
        Path::new(&safe_run_dir).join(&dir)
    };

    let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&info.output_file))?;
    create_file_with_dirs(&path, &code.as_bytes()).await?;
    for code in codes {
        let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&code.file))?;
        create_file_with_dirs(&path, &code.code.as_bytes()).await?;
    }

    // 以下のコマンドを実行してコンテナ内のユーザーが書き込めるようにする
    //
    // $ podman run --rm wandbox-runner id -u wandbox
    // 1001
    // $ podman run --rm wandbox-runner id -g wandbox
    // 1001
    // $ podman unshare chown -R 1001:1001 base_dir

    // まず uid と gid を取得
    let uid: Output = Command::new("podman")
        .arg("run")
        .arg("--rm")
        .arg(&image)
        .arg("id")
        .arg("-u")
        .arg("wandbox")
        .output()
        .await?;
    if !uid.status.success() {
        return Err(anyhow::anyhow!("Failed to get uid"));
    }
    let uid: String = String::from_utf8(uid.stdout)?.trim().to_string();
    let gid: Output = Command::new("podman")
        .arg("run")
        .arg("--rm")
        .arg(&image)
        .arg("id")
        .arg("-u")
        .arg("wandbox")
        .output()
        .await?;
    if !gid.status.success() {
        return Err(anyhow::anyhow!("Failed to get gid"));
    }
    let gid: String = String::from_utf8(gid.stdout)?.trim().to_string();
    let r: Output = Command::new("podman")
        .arg("unshare")
        .arg("chown")
        .arg("-R")
        .arg(format!("{}:{}", uid, gid))
        .arg(&base_dir)
        .output()
        .await?;
    if !r.status.success() {
        return Err(anyhow::anyhow!("Failed to unshare"));
    }
    Ok(base_dir)
}

pub async fn log_run_codes(
    config: &Config,
    safe_run_log_dir: &str,
    timestamp: &chrono::DateTime<chrono::Local>,
    unique_name: &str,
    compiler: &str,
    stdin: &[u8],
    compiler_option_raw: &str,
    runtime_option_raw: &str,
    compiler_options: &str,
    issuer: &Issuer,
    code: &str,
    codes: &Vec<Code>,
) -> Result<(), anyhow::Error> {
    let info: &Compiler = get_compiler(&config, &compiler)?;
    let run_log: RunLog = RunLog {
        compiler: compiler.to_string(),
        stdin: base64_encode(&stdin),
        compiler_option_raw: compiler_option_raw.to_string(),
        runtime_option_raw: runtime_option_raw.to_string(),
        compiler_options: compiler_options.to_string(),
        issuer: issuer.clone(),
    };
    let (base_base_dir, name) = {
        let date: String = timestamp.format("%Y%m%d").to_string();
        let fmtstr: String = timestamp.format("%Y%m%d_%H%M%S").to_string();
        let dir: String = format!("wandbox_{}_{}", fmtstr, unique_name);
        (Path::new(&safe_run_log_dir).join(&date), dir)
    };
    let json_file: String = name.clone() + ".json";
    let path: PathBuf = resolve_safe_path(&base_base_dir, &Path::new(&json_file))?;
    create_file_with_dirs(&path, &serde_json::to_string_pretty(&run_log)?.as_bytes()).await?;

    let base_dir: PathBuf = base_base_dir.join(&name);
    let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&info.output_file))?;
    create_file_with_dirs(&path, &code.as_bytes()).await?;
    for code in codes {
        let path: PathBuf = resolve_safe_path(&base_dir, &Path::new(&code.file))?;
        create_file_with_dirs(&path, &code.code.as_bytes()).await?;
    }

    Ok(())
}

pub fn run_compile_ndjson(
    config: Arc<AppConfig>,
    base_dir: PathBuf,
    body: CompileParameter,
) -> mpsc::Receiver<CompileNdjsonResult> {
    // プログラムを実行していく
    let (tx, rx) = mpsc::channel::<CompileNdjsonResult>(100);
    tokio::spawn(async move {
        // 気軽に await? できるようにしてるだけ
        let r: Result<(), anyhow::Error> = async move {
            let info: &Compiler = get_compiler(&config.config, &body.compiler)?;
            let args: Vec<String> = resolve_arguments(
                &config.config,
                &body.options,
                &body.compiler_option_raw,
                false,
            );
            let base_dir: PathBuf = base_dir.canonicalize()?;
            let base_dir_str: &str = base_dir.to_str().ok_or(anyhow::anyhow!("Invalid path"))?;
            let podman_config: PodmanConfig =
                config.podman.with_workdir(base_dir_str, "/home/wandbox");

            tx.send(CompileNdjsonResult {
                r#type: "Control".to_string(),
                data: b"Start".to_vec(),
            })
            .await?;

            // コンパイルフェーズ
            let mut crx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
                &podman_config,
                &info.compile_command[0],
                info.compile_command[1..]
                    .iter()
                    .cloned()
                    .chain(args)
                    .collect(),
                b"".to_vec(),
                Duration::from_secs(60),
            );
            let mut cont = true;
            while let Some(v) = crx.recv().await {
                // StdOut と StdErr を CompilerMessageS と CompilerMessageE に変換する
                let v = match v.r#type.as_str() {
                    "StdOut" => CompileNdjsonResult {
                        r#type: "CompilerMessageS".to_string(),
                        data: v.data,
                    },
                    "StdErr" => CompileNdjsonResult {
                        r#type: "CompilerMessageE".to_string(),
                        data: v.data,
                    },
                    _ => v,
                };
                // Signal か、ExitCode の 0 以外だったらコンパイルフェーズで終了
                if v.r#type == "Signal" || (v.r#type == "ExitCode" && v.data != b"0") {
                    cont = false;
                }
                // ExitCode 0 は送信しない
                if v.r#type == "ExitCode" && v.data == b"0" {
                    continue;
                }
                tx.send(v).await?;
            }
            if !cont {
                return Ok(());
            }

            // 実行フェーズ
            let args: Vec<String> = resolve_arguments(
                &config.config,
                &body.options,
                &body.runtime_option_raw,
                true,
            );
            let mut rrx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
                &podman_config,
                &info.run_command[0],
                info.run_command[1..].iter().cloned().chain(args).collect(),
                body.stdin,
                Duration::from_secs(60),
            );
            while let Some(v) = rrx.recv().await {
                tx.send(v).await?;
            }

            tx.send(CompileNdjsonResult {
                r#type: "Control".to_string(),
                data: b"Finish".to_vec(),
            })
            .await?;

            Ok(())
        }
        .await;

        if let Err(e) = r {
            log::error!("Error {}", e);
        }
    });
    rx
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        path::{Path, PathBuf},
        process::Output,
        vec,
    };

    #[tokio::test]
    async fn test_with_podman() {
        let mut cmd: Command = with_podman(
            &PodmanConfig::new("wandbox-runner"),
            "echo",
            &vec!["Hello".to_string()],
        );
        let output: Output = cmd.output().await.unwrap();
        assert_eq!(String::from_utf8(output.stdout), Ok("Hello\n".to_string()));
    }

    #[tokio::test]
    async fn test_run_streaming() {
        let mut rx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
            &PodmanConfig::new("wandbox-runner"),
            "bash",
            vec![
                "-c".to_string(),
                r#"
                    # 標準入力が正しく処理できるかの確認
                    cat
                    # 日時を出力して１秒待つのを２回やって、それぞれが違う文字になるかを確認する
                    date
                    sleep 1
                    date
                    sleep 1
                    # 標準エラーへの出力が正しく出てるかの確認
                    date >&2
                    sleep 1
                    # エラーを出して終了する
                    exit 3
                "#
                .to_string(),
            ],
            b"Stdin".to_vec(),
            Duration::from_secs(60),
        );
        let mut vs: Vec<CompileNdjsonResult> = vec![];
        while let Some(v) = rx.recv().await {
            vs.push(v);
        }
        assert_eq!(vs.len(), 5);
        assert_eq!(vs[0].r#type, "StdOut");
        assert_eq!(vs[0].data, b"Stdin");
        assert_eq!(vs[1].r#type, "StdOut");
        assert_eq!(vs[2].r#type, "StdOut");
        assert_eq!(vs[3].r#type, "StdErr");
        assert_ne!(vs[1].data, vs[2].data);
        assert_ne!(vs[2].data, vs[3].data);
        assert_eq!(vs[4].r#type, "ExitCode");
        assert_eq!(vs[4].data, b"3");
    }

    #[tokio::test]
    async fn test_run_streaming_signal() {
        let mut rx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
            &PodmanConfig::new("wandbox-runner"),
            "bash",
            vec![
                "-c".to_string(),
                r#"
                    kill -9 $$
                "#
                .to_string(),
            ],
            b"".to_vec(),
            Duration::from_secs(60),
        );
        let mut vs: Vec<CompileNdjsonResult> = vec![];
        while let Some(v) = rx.recv().await {
            vs.push(v);
        }
        assert_eq!(vs.len(), 1);
        // ほんとは SIGKILL を確認したかったけど podman 経由での実行だと
        // シグナルがちゃんと取得できないので、諦めて ExitCode で確認する。
        // 128 + 9(SIGKILL) = 137 が終了コードになる。
        assert_eq!(vs[0].r#type, "ExitCode");
        assert_eq!(vs[0].data, b"137");
    }

    #[tokio::test]
    async fn test_run_streaming_timeout() {
        let mut rx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
            &PodmanConfig::new("wandbox-runner"),
            "bash",
            vec![
                "-c".to_string(),
                r#"
                    sleep 10
                "#
                .to_string(),
            ],
            b"".to_vec(),
            // すぐ SIGKILL する
            Duration::from_secs(1),
        );
        let mut vs: Vec<CompileNdjsonResult> = vec![];
        while let Some(v) = rx.recv().await {
            vs.push(v);
        }
        assert_eq!(vs.len(), 1);
        assert_eq!(vs[0].r#type, "Control");
        assert_eq!(vs[0].data, b"Timeout");
    }

    #[tokio::test]
    async fn test_run_streaming_cpu() {
        // CPU 時間を使い切ると SIGKILL されることを確認する
        let mut config = PodmanConfig::new("wandbox-runner");
        config.cpu = 1;
        let mut rx: mpsc::Receiver<CompileNdjsonResult> = run_streaming(
            &config,
            "bash",
            vec![
                "-c".to_string(),
                r#"
                    yes > /dev/null
                "#
                .to_string(),
            ],
            b"".to_vec(),
            Duration::from_secs(30),
        );
        let mut vs: Vec<CompileNdjsonResult> = vec![];
        while let Some(v) = rx.recv().await {
            vs.push(v);
        }
        assert_eq!(vs.len(), 2);
        assert_eq!(vs[0].r#type, "StdErr");
        assert_eq!(
            vs[0].data,
            b"bash: line 2:     3 Killed                  yes > /dev/null\n"
        );
        assert_eq!(vs[1].r#type, "ExitCode");
        assert_eq!(vs[1].data, b"137");
    }

    #[tokio::test]
    async fn test_resolve_safe_path() {
        // dir が絶対パス
        {
            let dir: &Path = Path::new("/tmp");

            let file_path: &Path = Path::new("foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/foo/bar"));

            let file_path: &Path = Path::new("../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("../../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("foo/../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/bar"));

            let file_path: &Path = Path::new("foo/.././././../tmp/../tmp/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("/tmp/foo/bar"));

            let file_path: &Path = Path::new("/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("/tmp/foo");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");
        }

        // dir が相対パス
        {
            let dir: &Path = Path::new("tmp");

            let file_path: &Path = Path::new("foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/foo/bar"));

            let file_path: &Path = Path::new("../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("../../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("foo/../bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/bar"));

            let file_path: &Path = Path::new("foo/.././././../tmp/../tmp/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap(), Path::new("tmp/foo/bar"));

            let file_path: &Path = Path::new("/foo/bar");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");

            let file_path: &Path = Path::new("/tmp/foo");
            let path: Result<PathBuf, anyhow::Error> = resolve_safe_path(dir, file_path);
            assert_eq!(path.unwrap_err().to_string(), "Invalid path");
        }
    }
}
