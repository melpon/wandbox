use std::{
    collections::{HashMap, VecDeque},
    sync::Mutex,
    time::{Duration, Instant},
};

/// 同一 IP からのコンパイルリクエスト数を制限する。
/// 直近 `window` の間に `limit` 回まで許可する（スライディングウィンドウ）。
#[derive(Debug)]
pub struct CompileRateLimiter {
    limit: usize,
    window: Duration,
    // IP -> 直近 window 内のリクエスト時刻
    entries: Mutex<HashMap<String, VecDeque<Instant>>>,
}

impl Default for CompileRateLimiter {
    fn default() -> Self {
        // 通常の Hello World 実行が十数秒かかる前提で、10 req/分なら十分
        Self::new(10, Duration::from_secs(60))
    }
}

impl CompileRateLimiter {
    pub fn new(limit: usize, window: Duration) -> Self {
        Self {
            limit,
            window,
            entries: Mutex::new(HashMap::new()),
        }
    }

    /// 許可できれば true（カウントを消費）、超過なら false。
    pub fn try_acquire(&self, ip: &str) -> bool {
        let key: &str = if ip.is_empty() { "unknown" } else { ip };
        let now: Instant = Instant::now();
        let mut entries = self.entries.lock().unwrap();
        let times: &mut VecDeque<Instant> = entries.entry(key.to_string()).or_default();

        while let Some(front) = times.front() {
            if now.duration_since(*front) > self.window {
                times.pop_front();
            } else {
                break;
            }
        }

        if times.len() >= self.limit {
            return false;
        }
        times.push_back(now);

        // エントリが増えすぎないよう、空になった IP を時々掃除
        if entries.len() > 10_000 {
            entries.retain(|_, v| !v.is_empty());
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn allows_up_to_limit_then_blocks() {
        let limiter: CompileRateLimiter = CompileRateLimiter::new(3, Duration::from_secs(60));
        assert!(limiter.try_acquire("1.2.3.4"));
        assert!(limiter.try_acquire("1.2.3.4"));
        assert!(limiter.try_acquire("1.2.3.4"));
        assert!(!limiter.try_acquire("1.2.3.4"));
        // 別 IP は独立
        assert!(limiter.try_acquire("5.6.7.8"));
    }

    #[test]
    fn window_expires() {
        let limiter: CompileRateLimiter = CompileRateLimiter::new(1, Duration::from_millis(50));
        assert!(limiter.try_acquire("1.2.3.4"));
        assert!(!limiter.try_acquire("1.2.3.4"));
        thread::sleep(Duration::from_millis(60));
        assert!(limiter.try_acquire("1.2.3.4"));
    }

    #[test]
    fn empty_ip_shares_unknown_bucket() {
        let limiter: CompileRateLimiter = CompileRateLimiter::new(1, Duration::from_secs(60));
        assert!(limiter.try_acquire(""));
        assert!(!limiter.try_acquire(""));
        assert!(!limiter.try_acquire("unknown"));
    }
}
