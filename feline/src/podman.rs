use crate::types::{CompileNdjsonResult, PodmanConfig};
use anyhow::Result;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    process::Command,
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
    // $ podman run --rm --init -i \
    //     -v src:dst:ro -v src2:dst2 --workdir dst2 $image \
    //     bash -c 'ulimit -t $cpusec && exec "$@"' \
    //     _ $program $args
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

    xs.push(config.image.clone());

    // ここからはコンテナで実行するプログラムの引数
    let ulimit = format!(
        r#"ulimit -t {} && exec "$@""#,
        if config.cpusec > 0 { config.cpusec } else { 30 }
    );
    xs.extend(["bash", "-c", ulimit.as_str(), "_"].map(|x| x.to_string()));

    let mut cmd: Command = Command::new("podman");
    cmd.arg("run").args(xs).arg(&program).args(args);
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
        let r: Result<()> = async move {
            let mut command = with_podman(&config, &program, &args);
            let mut child = command
                .stdin(std::process::Stdio::piped())
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn()?;

            let mut stdin = child
                .stdin
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stdin"))?;
            let mut stdout = child
                .stdout
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stdout"))?;
            let mut stderr = child
                .stderr
                .take()
                .ok_or(anyhow::anyhow!("Failed to open stderr"))?;

            tokio::spawn(async move {
                let mut n = 0;
                while n < stdin_data.len() {
                    let end = std::cmp::min(n + 1024, stdin_data.len());
                    let r: Result<(), std::io::Error> = stdin.write_all(&stdin_data[n..end]).await;
                    if let Err(e) = r {
                        eprintln!("Error: {}", e);
                        return;
                    }
                    let r = stdin.flush().await;
                    if let Err(e) = r {
                        eprintln!("Error: {}", e);
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
                        eprintln!("Error: {}", e);
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
                        eprintln!("Error: {}", e);
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
                eprintln!("Error: {}", e);
            }
        }
    });

    return rx;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{process::Output, vec};

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
        config.cpusec = 1;
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
}
