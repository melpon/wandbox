import argparse
import logging
import os

from buildbase import (
    cd,
    cmd,
    mkdir_p,
    rm_rf,
)

logging.basicConfig(level=logging.DEBUG)


def replace_and_save(input_file, replacements, output_file):
    """
    ファイルの特定の文字列を置換して別名で保存する。

    :param input_file: 入力ファイルのパス
    :param replacements: 置換するタプルのリスト [(置換前, 置換後), ...]
    :param output_file: 出力ファイルのパス
    """
    content = open(input_file, "r", encoding="utf-8").read()
    for old, new in replacements:
        content = content.replace(old, new)
    open(output_file, "w", encoding="utf-8").write(content)


BASE_DIR = os.path.abspath(os.path.dirname(__file__))


def do_build(debug: bool, target: str, env: str):
    with cd(os.path.join(BASE_DIR, target)):
        cmd(["cargo", "build", *([] if debug else ["--release"])])


def do_package(debug: bool, target: str, env: str, prefix: str):
    package_dir = os.path.join(BASE_DIR, "_package")
    feline_package_dir = os.path.join(package_dir, f"{target}-{env}")
    rm_rf(feline_package_dir)
    mkdir_p(feline_package_dir)

    do_build(debug, target, env)
    with cd(os.path.join(BASE_DIR, "feline")):
        # このあたりは普通にコピー
        cmd(["cp", f"target/{'debug' if debug else 'release'}/feline", feline_package_dir])
        cmd(["cp", "conf/podman.json", feline_package_dir])
        cmd(["cp", "conf/sponsors.json", feline_package_dir])
        cmd(["cp", "-r", "Containerfiles", f"{feline_package_dir}/Containerfiles"])
        cmd(["cp", "-r", "migrations", f"{feline_package_dir}/migrations"])
        # これは置換してコピー
        if env == "master":
            replace_and_save(
                "conf/feline.service",
                [
                    ("@FELINE_DIR@", f"{prefix}/{target}-{env}"),
                    ("@FELINE_BIND_ADDR@", "127.0.0.1:3500"),
                    ("@FELINE_SAFE_RUN_DIR@", "/tmp/wandbox"),
                    ("@FELINE_SAFE_RUN_LOG_DIR@", "/opt/wandbox/_log/ran"),
                    ("@WANDBOX_URL@", "https://wandbox.org/"),
                ],
                f"{feline_package_dir}/feline.service",
            )
        elif env == "develop":
            replace_and_save(
                "conf/feline.service",
                [
                    ("@FELINE_DIR@", f"{prefix}/{target}-{env}"),
                    ("@FELINE_BIND_ADDR@", "127.0.0.1:3501"),
                    ("@FELINE_SAFE_RUN_DIR@", "/tmp/wandbox-develop"),
                    ("@FELINE_SAFE_RUN_LOG_DIR@", "/tmp/wandbox-develop-log"),
                    ("@WANDBOX_URL@", "https://develop.wandbox.org/"),
                ],
                f"{feline_package_dir}/feline.service",
            )
    with cd(package_dir):
        cmd(["tar", "czf", f"{target}-{env}.tar.gz", f"{target}-{env}"])


def do_deploy(remote: str, target: str, env: str, prefix: str):
    package_dir = os.path.join(BASE_DIR, "_package")

    cmd(
        [
            "scp",
            os.path.join(package_dir, f"{target}-{env}.tar.gz"),
            f"{remote}:/tmp/{target}-{env}.tar.gz",
        ]
    )
    remote_command = f"""
set -ex
mkdir -p {prefix}
pushd {prefix}
  tar xf /tmp/{target}-{env}.tar.gz
  rm /tmp/{target}-{env}.tar.gz
  pushd {target}-{env}
    mkdir -p var
    chown -R ubuntu:ubuntu var/
  popd
popd
cp /opt/wandbox-data/release/{target}-{env}/{target}.service /etc/systemd/system/{target}-{env}.service
systemctl enable {target}-{env}
systemctl restart {target}-{env}
"""
    cmd(["ssh", remote, "/bin/bash", "-c", remote_command])


PACKAGE_PREFIX = "/opt/wandbox-data/release"


def main():
    parser = argparse.ArgumentParser()
    sp = parser.add_subparsers()
    bp = sp.add_parser("build")
    bp.set_defaults(op="build")
    bp.add_argument("target", choices=["feline"])
    bp.add_argument("--env", choices=["master", "develop"], required=True)
    bp.add_argument("--debug", action="store_true")
    pp = sp.add_parser("package")
    pp.set_defaults(op="package")
    pp.add_argument("target", choices=["feline"])
    pp.add_argument("--env", choices=["master", "develop"], required=True)
    pp.add_argument("--debug", action="store_true")
    pp.add_argument("--prefix", default=PACKAGE_PREFIX)
    dp = sp.add_parser("deploy")
    dp.set_defaults(op="deploy")
    dp.add_argument("remote")
    dp.add_argument("target", choices=["feline"])
    dp.add_argument("--env", choices=["master", "develop"], required=True)
    dp.add_argument("--prefix", default=PACKAGE_PREFIX)

    args = parser.parse_args()

    if args.op == "build":
        do_build(debug=args.debug, target=args.target, env=args.env)
    elif args.op == "package":
        do_package(debug=args.debug, target=args.target, env=args.env, prefix=args.prefix)
    elif args.op == "deploy":
        do_deploy(remote=args.remote, target=args.target, env=args.env, prefix=args.prefix)


if __name__ == "__main__":
    main()
