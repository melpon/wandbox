use crate::types::{CompileNdjsonResult, PodmanConfig};
use anyhow::Result;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    process::Command,
    sync::mpsc,
    time::{Duration, sleep},
};

impl PodmanConfig {
    pub fn new(image: &str) -> PodmanConfig {
        PodmanConfig {
            image: image.to_string(),
            ..PodmanConfig::default()
        }
    }
}

pub fn with_podman(config: &PodmanConfig, program: &str, args: &[String]) -> Command {
    // podman run --rm --init -i $image -v src:dst -v src2:dst2 $program $args
    // let mut args: Vec<String> = vec![];
    let mut cmd: Command = Command::new("podman");
    cmd.arg("run")
        .arg("--rm")
        .arg("--init")
        .arg("-i")
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
}
