import argparse
import logging
import multiprocessing
import os

from buildbase import (
    add_path,
    build_and_install_boost,
    cd,
    cmd,
    install_cli11,
    install_cmake,
    install_cppdb,
    install_ggrpc,
    install_grpc,
    install_protoc_gen_jsonif,
    install_spdlog,
    install_sqlite3,
    mkdir_p,
    read_version_file,
    rm_rf,
)

logging.basicConfig(level=logging.DEBUG)


BASE_DIR = os.path.abspath(os.path.dirname(__file__))


def install_deps(source_dir: str, build_dir: str, install_dir: str, debug: bool):
    with cd(BASE_DIR):
        version = read_version_file("VERSION")

        # CMake
        install_cmake_args = {
            "version": version["CMAKE_VERSION"],
            "version_file": os.path.join(install_dir, "cmake.version"),
            "source_dir": source_dir,
            "install_dir": install_dir,
            "platform": "linux-x86_64",
            "ext": "tar.gz",
        }
        install_cmake(**install_cmake_args)
        add_path(os.path.join(install_dir, "cmake", "bin"))

        # gRPC
        install_grpc_args = {
            "version": version["GRPC_VERSION"],
            "version_file": os.path.join(install_dir, "grpc.version"),
            "source_dir": source_dir,
            "build_dir": build_dir,
            "install_dir": install_dir,
            "debug": debug,
            "cmake_args": [],
        }
        install_grpc(**install_grpc_args)

        # ggrpc
        install_ggrpc_args = {
            "version": version["GGRPC_VERSION"],
            "version_file": os.path.join(install_dir, "ggrpc.version"),
            "install_dir": install_dir,
        }
        install_ggrpc(**install_ggrpc_args)

        # spdlog
        install_spdlog_args = {
            "version": version["SPDLOG_VERSION"],
            "version_file": os.path.join(install_dir, "spdlog.version"),
            "install_dir": install_dir,
        }
        install_spdlog(**install_spdlog_args)

        # Boost
        install_boost_args = {
            "version": version["BOOST_VERSION"],
            "version_file": os.path.join(install_dir, "boost.version"),
            "source_dir": source_dir,
            "build_dir": build_dir,
            "install_dir": install_dir,
            "cxx": "",
            "cflags": [],
            "cxxflags": [],
            "linkflags": [],
            "toolset": "gcc",
            "visibility": "global",
            "target_os": "linux",
            "debug": debug,
            "android_ndk": "",
            "native_api_level": "",
            "architecture": "x86",
        }
        build_and_install_boost(**install_boost_args)

        # CLI11
        install_cli11_args = {
            "version": version["CLI11_VERSION"],
            "version_file": os.path.join(install_dir, "cli11.version"),
            "install_dir": install_dir,
        }
        install_cli11(**install_cli11_args)

        # protoc-gen-jsonif
        install_jsonif_args = {
            "version": version["PROTOC_GEN_JSONIF_VERSION"],
            "version_file": os.path.join(install_dir, "protoc-gen-jsonif.version"),
            "source_dir": source_dir,
            "install_dir": install_dir,
            "platform": "linux-amd64",
        }
        install_protoc_gen_jsonif(**install_jsonif_args)

        # SQLite3
        install_sqlite3_args = {
            "version": version["SQLITE3_VERSION"],
            "version_file": os.path.join(install_dir, "sqlite3.version"),
            "year": version["SQLITE3_YEAR"],
            "source_dir": source_dir,
            "build_dir": build_dir,
            "install_dir": install_dir,
            "debug": debug,
            "configure_args": [],
        }
        install_sqlite3(**install_sqlite3_args)

        # CppDB
        install_cppdb_args = {
            "version": version["CPPDB_VERSION"],
            "version_file": os.path.join(install_dir, "cppdb.version"),
            "source_dir": source_dir,
            "build_dir": build_dir,
            "install_dir": install_dir,
            "debug": debug,
            "sqlite3_install_dir": os.path.join(install_dir, "sqlite3"),
            "cmake_args": [],
        }
        install_cppdb(**install_cppdb_args)


def do_build(debug: bool, target: str, cattleshed_install_dir=None, kennel_install_dir=None):
    configuration = "debug" if debug else "release"
    cmake_configuration = "Debug" if debug else "Release"
    source_dir = os.path.join(BASE_DIR, "_source", configuration)
    build_dir = os.path.join(BASE_DIR, "_build", configuration)
    install_dir = os.path.join(BASE_DIR, "_install", configuration)
    # package_dir = os.path.join(BASE_DIR, "_package", dir, configuration)
    mkdir_p(source_dir)
    mkdir_p(build_dir)
    mkdir_p(install_dir)

    install_deps(source_dir, build_dir, install_dir, debug)

    if target == "cattleshed":
        cattleshed_source_dir = os.path.join(BASE_DIR, "cattleshed")
        cattleshed_build_dir = os.path.join(build_dir, "cattleshed")
        if cattleshed_install_dir is None:
            cattleshed_install_dir = os.path.join(install_dir, "cattleshed")
        mkdir_p(cattleshed_build_dir)
        with cd(cattleshed_build_dir):
            cmd(
                [
                    "cmake",
                    cattleshed_source_dir,
                    f"-DCLI11_ROOT_DIR={os.path.join(install_dir, 'cli11')}",
                    f"-DSPDLOG_ROOT_DIR={os.path.join(install_dir, 'spdlog')}",
                    f"-DGGRPC_ROOT_DIR={os.path.join(install_dir, 'ggrpc')}",
                    f"-DCMAKE_PREFIX_PATH={os.path.join(install_dir, 'boost')};{os.path.join(install_dir, 'grpc')}",
                    f"-DCMAKE_MODULE_PATH={os.path.join(BASE_DIR, 'cmake')}",
                    f"-DCMAKE_INSTALL_PREFIX={cattleshed_install_dir}",
                    f"-DCMAKE_BUILD_TYPE={cmake_configuration}",
                ]
            )
            cmd(
                [
                    "cmake",
                    "--build",
                    ".",
                    "--config",
                    cmake_configuration,
                    f"-j{multiprocessing.cpu_count()}",
                ]
            )

        # 各種権限を設定
        cmd(
            [
                "sudo",
                "setcap",
                "cap_sys_admin,cap_chown,cap_setuid,cap_setgid,cap_sys_chroot,cap_mknod,cap_net_admin=p",
                os.path.join(cattleshed_build_dir, "cattlegrid"),
            ]
        )

    elif target == "kennel":
        kennel_source_dir = os.path.join(BASE_DIR, "kennel")
        kennel_build_dir = os.path.join(build_dir, "kennel")
        if kennel_install_dir is None:
            kennel_install_dir = os.path.join(install_dir, "kennel")
        mkdir_p(kennel_build_dir)
        with cd(kennel_build_dir):
            cmd(
                [
                    "cmake",
                    kennel_source_dir,
                    f"-DCLI11_ROOT_DIR={os.path.join(install_dir, 'cli11')}",
                    f"-DSPDLOG_ROOT_DIR={os.path.join(install_dir, 'spdlog')}",
                    f"-DGGRPC_ROOT_DIR={os.path.join(install_dir, 'ggrpc')}",
                    f"-DCMAKE_PREFIX_PATH={os.path.join(install_dir, 'boost')};{os.path.join(install_dir, 'grpc')};{os.path.join(install_dir, 'cppdb')};{os.path.join(install_dir, 'sqlite3')}",
                    f"-DCMAKE_MODULE_PATH={os.path.join(BASE_DIR, 'cmake')}",
                    f"-DCMAKE_INSTALL_PREFIX={kennel_install_dir}",
                    f"-DCMAKE_BUILD_TYPE={cmake_configuration}",
                    f"-DPROTOC_GEN_JSONIF_DIR={os.path.join(install_dir, 'protoc-gen-jsonif')}",
                    f"-DPROTOC_GEN_JSONIF_CPP={os.path.join(install_dir, 'protoc-gen-jsonif', 'bin', 'protoc-gen-jsonif-cpp')}",
                    f"-DCppDB_ROOT_DIR={os.path.join(install_dir, 'cppdb')}",
                    f"-DSQLite3_INCLUDE_DIR={os.path.join(install_dir, 'sqlite3', 'include')}",
                ]
            )
            cmd(
                [
                    "cmake",
                    "--build",
                    ".",
                    "--config",
                    cmake_configuration,
                    f"-j{multiprocessing.cpu_count()}",
                ]
            )


def do_package(debug: bool, target: str, env: str, prefix: str):
    configuration = "debug" if debug else "release"
    cmake_configuration = "Debug" if debug else "Release"
    build_dir = os.path.join(BASE_DIR, "_build", configuration)
    package_dir = os.path.join(BASE_DIR, "_package")
    mkdir_p(package_dir)
    do_build(debug, target, f"{prefix}/cattleshed-{env}", f"{prefix}/kennel-{env}")
    cmd(["cmake", "--install", os.path.join(build_dir, target), "--config", cmake_configuration])
    with cd(prefix):
        cmd(["tar", "czf", os.path.join(package_dir, f"{target}-{env}.tar.gz"), f"{target}-{env}"])
    rm_rf(os.path.join(prefix, f"{target}-{env}"))


PACKAGE_PREFIX = "/opt/wandbox-data/release"


def main():
    parser = argparse.ArgumentParser()
    sp = parser.add_subparsers()
    bp = sp.add_parser("build")
    bp.set_defaults(op="build")
    bp.add_argument("target", choices=["kennel", "cattleshed"])
    bp.add_argument("--debug", action="store_true")
    pp = sp.add_parser("package")
    pp.set_defaults(op="package")
    pp.add_argument("target", choices=["kennel", "cattleshed"])
    pp.add_argument("--env", choices=["master", "develop"], required=True)
    pp.add_argument("--debug", action="store_true")
    pp.add_argument("--prefix", default=PACKAGE_PREFIX)

    args = parser.parse_args()

    if args.op == "build":
        do_build(debug=args.debug, target=args.target)
    elif args.op == "package":
        do_package(debug=args.debug, target=args.target, env=args.env, prefix=args.prefix)


if __name__ == "__main__":
    main()
