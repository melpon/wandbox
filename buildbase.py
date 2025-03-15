# buildbase.py はビルドスクリプトのテンプレートとなるファイル
#
# 自身のリポジトリにコピーして利用する。
#
# 元のファイルは以下のリポジトリにある:
# https://github.com/melpon/buildbase
#
# 更新する場合は以下のコマンドを利用する:
# curl -LO https://raw.githubusercontent.com/melpon/buildbase/master/buildbase.py
#
# ライセンス: Apache License 2.0
#
# Copyright 2024 melpon (Wandbox)
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import filecmp
import glob
import logging
import multiprocessing
import os
import platform
import shlex
import shutil
import stat
import subprocess
import tarfile
import urllib.parse
import zipfile
from typing import Dict, List, NamedTuple, Optional

if platform.system() == "Windows":
    import winreg


class ChangeDirectory(object):
    def __init__(self, cwd):
        self._cwd = cwd

    def __enter__(self):
        self._old_cwd = os.getcwd()
        logging.debug(f"pushd {self._old_cwd} --> {self._cwd}")
        os.chdir(self._cwd)

    def __exit__(self, exctype, excvalue, trace):
        logging.debug(f"popd {self._old_cwd} <-- {self._cwd}")
        os.chdir(self._old_cwd)
        return False


def cd(cwd):
    return ChangeDirectory(cwd)


def cmd(args, **kwargs):
    logging.debug(f"+{args} {kwargs}")
    if "check" not in kwargs:
        kwargs["check"] = True
    if "resolve" in kwargs:
        resolve = kwargs["resolve"]
        del kwargs["resolve"]
    else:
        resolve = True
    if resolve:
        args = [shutil.which(args[0]), *args[1:]]
    return subprocess.run(args, **kwargs)


# 標準出力をキャプチャするコマンド実行。シェルの `cmd ...` や $(cmd ...) と同じ
def cmdcap(args, **kwargs):
    # 3.7 でしか使えない
    # kwargs['capture_output'] = True
    kwargs["stdout"] = subprocess.PIPE
    kwargs["stderr"] = subprocess.PIPE
    kwargs["encoding"] = "utf-8"
    return cmd(args, **kwargs).stdout.strip()


# https://stackoverflow.com/a/2656405
def onerror(func, path, exc_info):
    """
    Error handler for ``shutil.rmtree``.
    If the error is due to an access error (read only file)
    it attempts to add write permission and then retries.
    If the error is for another reason it re-raises the error.

    Usage : ``shutil.rmtree(path, onerror=onerror)``
    """
    import stat

    # Is the error an access error?
    if not os.access(path, os.W_OK):
        os.chmod(path, stat.S_IWUSR)
        func(path)
    else:
        raise


def rm_rf(path: str):
    if not os.path.exists(path):
        logging.debug(f"rm -rf {path} => path not found")
        return
    if os.path.isfile(path) or os.path.islink(path):
        os.remove(path)
        logging.debug(f"rm -rf {path} => file removed")
    if os.path.isdir(path):
        shutil.rmtree(path, onerror=onerror)
        logging.debug(f"rm -rf {path} => directory removed")


def mkdir_p(path: str):
    if os.path.exists(path):
        logging.debug(f"mkdir -p {path} => already exists")
        return
    os.makedirs(path, exist_ok=True)
    logging.debug(f"mkdir -p {path} => directory created")


if platform.system() == "Windows":
    PATH_SEPARATOR = ";"
else:
    PATH_SEPARATOR = ":"


def add_path(path: str, is_after=False):
    logging.debug(f"add_path: {path}")
    if "PATH" not in os.environ:
        os.environ["PATH"] = path
        return

    if is_after:
        os.environ["PATH"] = os.environ["PATH"] + PATH_SEPARATOR + path
    else:
        os.environ["PATH"] = path + PATH_SEPARATOR + os.environ["PATH"]


def download(url: str, output_dir: Optional[str] = None, filename: Optional[str] = None) -> str:
    if filename is None:
        output_path = urllib.parse.urlparse(url).path.split("/")[-1]
    else:
        output_path = filename

    if output_dir is not None:
        output_path = os.path.join(output_dir, output_path)

    if os.path.exists(output_path):
        return output_path

    try:
        if shutil.which("curl") is not None:
            cmd(["curl", "-fLo", output_path, url])
        else:
            cmd(["wget", "-cO", output_path, url])
    except Exception:
        # ゴミを残さないようにする
        if os.path.exists(output_path):
            os.remove(output_path)
        raise

    return output_path


def read_version_file(path: str) -> Dict[str, str]:
    versions = {}

    lines = open(path, encoding="utf-8").readlines()
    for line in lines:
        line = line.strip()

        # コメント行
        if line[:1] == "#":
            continue

        # 空行
        if len(line) == 0:
            continue

        [a, b] = map(lambda x: x.strip(), line.split("=", 2))
        versions[a] = b.strip('"')

    return versions


# dir 以下にある全てのファイルパスを、dir2 からの相対パスで返す
def enum_all_files(dir, dir2):
    for root, _, files in os.walk(dir):
        for file in files:
            yield os.path.relpath(os.path.join(root, file), dir2)


def versioned(func):
    def wrapper(version, version_file, *args, **kwargs):
        if "ignore_version" in kwargs:
            if kwargs.get("ignore_version"):
                rm_rf(version_file)
            del kwargs["ignore_version"]

        if os.path.exists(version_file):
            ver = open(version_file, encoding="utf-8").read()
            if ver.strip() == version.strip():
                return

        r = func(version=version, *args, **kwargs)

        with open(version_file, "w", encoding="utf-8") as f:
            f.write(version)

        return r

    return wrapper


# アーカイブが単一のディレクトリに全て格納されているかどうかを調べる。
#
# 単一のディレクトリに格納されている場合はそのディレクトリ名を返す。
# そうでない場合は None を返す。
def _is_single_dir(infos, get_name, is_dir) -> Optional[str]:
    # tarfile: ['path', 'path/to', 'path/to/file.txt']
    # zipfile: ['path/', 'path/to/', 'path/to/file.txt']
    # どちらも / 区切りだが、ディレクトリの場合、後ろに / が付くかどうかが違う
    dirname = None
    for info in infos:
        name = get_name(info)
        n = name.rstrip("/").find("/")
        if n == -1:
            # ルートディレクトリにファイルが存在している
            if not is_dir(info):
                return None
            dir = name.rstrip("/")
        else:
            dir = name[0:n]
        # ルートディレクトリに２個以上のディレクトリが存在している
        if dirname is not None and dirname != dir:
            return None
        dirname = dir

    return dirname


def is_single_dir_tar(tar: tarfile.TarFile) -> Optional[str]:
    return _is_single_dir(tar.getmembers(), lambda t: t.name, lambda t: t.isdir())


def is_single_dir_zip(zip: zipfile.ZipFile) -> Optional[str]:
    return _is_single_dir(zip.infolist(), lambda z: z.filename, lambda z: z.is_dir())


# 解凍した上でファイル属性を付与する
def _extractzip(z: zipfile.ZipFile, path: str):
    z.extractall(path)
    if platform.system() == "Windows":
        return
    for info in z.infolist():
        if info.is_dir():
            continue
        filepath = os.path.join(path, info.filename)
        mod = info.external_attr >> 16
        if (mod & 0o120000) == 0o120000:
            # シンボリックリンク
            with open(filepath, "r", encoding="utf-8") as f:
                src = f.read()
            os.remove(filepath)
            with cd(os.path.dirname(filepath)):
                if os.path.exists(src):
                    os.symlink(src, filepath)
        if os.path.exists(filepath):
            # 普通のファイル
            os.chmod(filepath, mod & 0o777)


# zip または tar.gz ファイルを展開する。
#
# 展開先のディレクトリは {output_dir}/{output_dirname} となり、
# 展開先のディレクトリが既に存在していた場合は削除される。
#
# もしアーカイブの内容が単一のディレクトリであった場合、
# そのディレクトリは無いものとして展開される。
#
# つまりアーカイブ libsora-1.23.tar.gz の内容が
# ['libsora-1.23', 'libsora-1.23/file1', 'libsora-1.23/file2']
# であった場合、extract('libsora-1.23.tar.gz', 'out', 'libsora') のようにすると
# - out/libsora/file1
# - out/libsora/file2
# が出力される。
#
# また、アーカイブ libsora-1.23.tar.gz の内容が
# ['libsora-1.23', 'libsora-1.23/file1', 'libsora-1.23/file2', 'LICENSE']
# であった場合、extract('libsora-1.23.tar.gz', 'out', 'libsora') のようにすると
# - out/libsora/libsora-1.23/file1
# - out/libsora/libsora-1.23/file2
# - out/libsora/LICENSE
# が出力される。
def extract(file: str, output_dir: str, output_dirname: str, filetype: Optional[str] = None):
    path = os.path.join(output_dir, output_dirname)
    logging.info(f"Extract {file} to {path}")
    if filetype == "gzip" or file.endswith(".tar.gz"):
        rm_rf(path)
        with tarfile.open(file) as t:
            dir = is_single_dir_tar(t)
            if dir is None:
                os.makedirs(path, exist_ok=True)
                t.extractall(path)
            else:
                logging.info(f"Directory {dir} is stripped")
                path2 = os.path.join(output_dir, dir)
                rm_rf(path2)
                t.extractall(output_dir)
                if path != path2:
                    logging.debug(f"mv {path2} {path}")
                    os.replace(path2, path)
    elif filetype == "zip" or file.endswith(".zip"):
        rm_rf(path)
        with zipfile.ZipFile(file) as z:
            dir = is_single_dir_zip(z)
            if dir is None:
                os.makedirs(path, exist_ok=True)
                # z.extractall(path)
                _extractzip(z, path)
            else:
                logging.info(f"Directory {dir} is stripped")
                path2 = os.path.join(output_dir, dir)
                rm_rf(path2)
                # z.extractall(output_dir)
                _extractzip(z, output_dir)
                if path != path2:
                    logging.debug(f"mv {path2} {path}")
                    os.replace(path2, path)
    else:
        raise Exception("file should end with .tar.gz or .zip")


def clone_and_checkout(url, version, dir, fetch, fetch_force):
    if fetch_force:
        rm_rf(dir)

    if not os.path.exists(os.path.join(dir, ".git")):
        cmd(["git", "clone", url, dir])
        fetch = True

    if fetch:
        with cd(dir):
            cmd(["git", "fetch"])
            cmd(["git", "reset", "--hard"])
            cmd(["git", "clean", "-df"])
            cmd(["git", "checkout", "-f", version])


def git_clone_shallow(url, hash, dir, submodule=False):
    rm_rf(dir)
    mkdir_p(dir)
    with cd(dir):
        cmd(["git", "init"])
        cmd(["git", "remote", "add", "origin", url])
        cmd(["git", "fetch", "--depth=1", "origin", hash])
        cmd(["git", "reset", "--hard", "FETCH_HEAD"])
        if submodule:
            cmd(
                [
                    "git",
                    "submodule",
                    "update",
                    "--init",
                    "--recursive",
                    "--recommend-shallow",
                    "--depth",
                    "1",
                ]
            )


def apply_patch(patch, dir, depth):
    with cd(dir):
        logging.info(f"patch -p{depth} < {patch}")
        if platform.system() == "Windows":
            cmd(
                [
                    "git",
                    "apply",
                    f"-p{depth}",
                    "--ignore-space-change",
                    "--ignore-whitespace",
                    "--whitespace=nowarn",
                    "--reject",
                    patch,
                ]
            )
        else:
            with open(patch, encoding="utf-8") as stdin:
                cmd(["patch", f"-p{depth}"], stdin=stdin)


def apply_patch_text(patch_text, dir, depth):
    with cd(dir):
        logging.info(f"echo '{patch_text[:100]}...' | patch -p{depth} -")
        directory = cmdcap(["git", "rev-parse", "--show-prefix"])
        if platform.system() == "Windows":
            cmd(
                [
                    "git",
                    "apply",
                    f"-p{depth}",
                    "--ignore-space-change",
                    "--ignore-whitespace",
                    "--whitespace=nowarn",
                    "--reject",
                    f"--directory={directory}",
                    "-",
                ],
                input=patch_text,
                text=True,
                encoding="utf-8",
            )
        else:
            cmd(["patch", f"-p{depth}"], input=patch_text, text=True, encoding="utf-8")


def copyfile_if_different(src, dst):
    if os.path.exists(dst) and filecmp.cmp(src, dst, shallow=False):
        return
    shutil.copyfile(src, dst)


# NOTE(enm10k): shutil.copytree に Python 3.8 で追加された dirs_exist_ok=True を指定して使いたかったが、
# GitHub Actions の Windows のランナー (widnwos-2019) にインストールされている Python のバージョンが古くて利用できなかった
# actions/setup-python で Python 3.8 を設定してビルドしたところ、 Lyra のビルドがエラーになったためこの関数を自作した
# Windows のランナーを更新した場合は、この関数は不要になる可能性が高い
def copytree(src_dir, dst_dir):
    for file_path in glob.glob(src_dir + "/**", recursive=True):
        dest_path = os.path.join(dst_dir, os.path.relpath(file_path, src_dir))

        if os.path.isdir(file_path):
            os.makedirs(dest_path, exist_ok=True)
        else:
            shutil.copy2(file_path, dest_path)


def git_get_url_and_revision(dir):
    with cd(dir):
        rev = cmdcap(["git", "rev-parse", "HEAD"])
        url = cmdcap(["git", "remote", "get-url", "origin"])
        return url, rev


def replace_vcproj_static_runtime(project_file: str):
    # なぜか MSVC_STATIC_RUNTIME が効かずに DLL ランタイムを使ってしまうので
    # 生成されたプロジェクトに対して静的ランタイムを使うように変更する
    s = open(project_file, "r", encoding="utf-8").read()
    s = s.replace("MultiThreadedDLL", "MultiThreaded")
    s = s.replace("MultiThreadedDebugDLL", "MultiThreadedDebug")
    open(project_file, "w", encoding="utf-8").write(s)


@versioned
def install_webrtc(version, source_dir, install_dir, platform: str):
    win = platform.startswith("windows_")
    filename = f"webrtc.{platform}.{'zip' if win else 'tar.gz'}"
    rm_rf(os.path.join(source_dir, filename))
    archive = download(
        f"https://github.com/shiguredo-webrtc-build/webrtc-build/releases/download/{version}/{filename}",
        output_dir=source_dir,
    )
    rm_rf(os.path.join(install_dir, "webrtc"))
    extract(archive, output_dir=install_dir, output_dirname="webrtc")


def build_webrtc(platform, local_webrtc_build_dir, local_webrtc_build_args, debug):
    with cd(local_webrtc_build_dir):
        args = ["--webrtc-nobuild-ios-framework", "--webrtc-nobuild-android-aar"]
        if debug:
            args += ["--debug"]

        args += local_webrtc_build_args

        cmd(["python3", "run.py", "build", platform, *args])

        # インクルードディレクトリを増やしたくないので、
        # __config_site を libc++ のディレクトリにコピーしておく
        webrtc_source_dir = os.path.join(local_webrtc_build_dir, "_source", platform, "webrtc")
        src_config = os.path.join(
            webrtc_source_dir, "src", "buildtools", "third_party", "libc++", "__config_site"
        )
        dst_config = os.path.join(
            webrtc_source_dir, "src", "third_party", "libc++", "src", "include", "__config_site"
        )
        copyfile_if_different(src_config, dst_config)

        # __assertion_handler をコピーする
        src_assertion = os.path.join(
            webrtc_source_dir,
            "src",
            "buildtools",
            "third_party",
            "libc++",
            "__assertion_handler",
        )
        dst_assertion = os.path.join(
            webrtc_source_dir,
            "src",
            "third_party",
            "libc++",
            "src",
            "include",
            "__assertion_handler",
        )
        copyfile_if_different(src_assertion, dst_assertion)


class WebrtcInfo(NamedTuple):
    version_file: str
    deps_file: str
    webrtc_include_dir: str
    webrtc_source_dir: Optional[str]
    webrtc_library_dir: str
    clang_dir: str
    libcxx_dir: str


def get_webrtc_info(
    platform: str, local_webrtc_build_dir: Optional[str], install_dir: str, debug: bool
) -> WebrtcInfo:
    webrtc_install_dir = os.path.join(install_dir, "webrtc")

    if local_webrtc_build_dir is None:
        return WebrtcInfo(
            version_file=os.path.join(webrtc_install_dir, "VERSIONS"),
            deps_file=os.path.join(webrtc_install_dir, "DEPS"),
            webrtc_include_dir=os.path.join(webrtc_install_dir, "include"),
            webrtc_source_dir=None,
            webrtc_library_dir=os.path.join(webrtc_install_dir, "lib"),
            clang_dir=os.path.join(install_dir, "llvm", "clang"),
            libcxx_dir=os.path.join(install_dir, "llvm", "libcxx"),
        )
    else:
        webrtc_build_source_dir = os.path.join(
            local_webrtc_build_dir, "_source", platform, "webrtc"
        )
        configuration = "debug" if debug else "release"
        webrtc_build_build_dir = os.path.join(
            local_webrtc_build_dir, "_build", platform, configuration, "webrtc"
        )

        return WebrtcInfo(
            version_file=os.path.join(local_webrtc_build_dir, "VERSION"),
            deps_file=os.path.join(local_webrtc_build_dir, "DEPS"),
            webrtc_include_dir=os.path.join(webrtc_build_source_dir, "src"),
            webrtc_source_dir=os.path.join(webrtc_build_source_dir, "src"),
            webrtc_library_dir=webrtc_build_build_dir,
            clang_dir=os.path.join(
                webrtc_build_source_dir, "src", "third_party", "llvm-build", "Release+Asserts"
            ),
            libcxx_dir=os.path.join(webrtc_build_source_dir, "src", "third_party", "libc++", "src"),
        )


@versioned
def install_boost(version, source_dir, install_dir, sora_version, platform: str):
    win = platform.startswith("windows_")
    filename = (
        f"boost-{version}_sora-cpp-sdk-{sora_version}_{platform}.{'zip' if win else 'tar.gz'}"
    )
    rm_rf(os.path.join(source_dir, filename))
    archive = download(
        f"https://github.com/shiguredo/sora-cpp-sdk/releases/download/{sora_version}/{filename}",
        output_dir=source_dir,
    )
    rm_rf(os.path.join(install_dir, "boost"))
    extract(archive, output_dir=install_dir, output_dirname="boost")


# 以下の問題を解決するためのパッチ
#
# No support for msvc-toolset 14.4x (VS 2022, 17.10.x): https://github.com/boostorg/boost/issues/914
BOOST_PATCH_SUPPORT_14_4 = r"""
diff --git a/tools/build/src/engine/config_toolset.bat b/tools/build/src/engine/config_toolset.bat
index 4ba577cac..3e3f6a3a1 100644
--- a/tools/build/src/engine/config_toolset.bat
+++ b/tools/build/src/engine/config_toolset.bat
@@ -157,7 +157,7 @@ pushd %CD%
 if "_%VSINSTALLDIR%_" == "__" call :Call_If_Exists "%B2_TOOLSET_ROOT%Auxiliary\Build\vcvarsall.bat" %B2_BUILD_ARGS%
 popd
 @REM set "B2_CXX="%CXX%" /nologo /MP /MT /TP /Feb2 /wd4996 /O2 /GL /EHsc"
-set "B2_CXX="%CXX%" /nologo -TP /wd4996 /wd4675 /EHs /GR /Zc:throwingNew /O2 /Ob2 /W3 /MD /Zc:forScope /Zc:wchar_t /Zc:inline /Gw /favor:blend /Feb2"
+set "B2_CXX="%CXX%" /nologo -TP /wd4996 /wd4675 /EHs /GR /Zc:throwingNew /O2 /Ob2 /W3 /MT /Zc:forScope /Zc:wchar_t /Zc:inline /Gw /favor:blend /Feb2"
 set "B2_CXX_LINK=/link kernel32.lib advapi32.lib user32.lib"
 set "_known_=1"
 goto :Embed_Minafest_Via_Link
diff --git a/tools/build/src/tools/msvc.jam b/tools/build/src/tools/msvc.jam
index 54a6ced32..4bb3810b3 100644
--- a/tools/build/src/tools/msvc.jam
+++ b/tools/build/src/tools/msvc.jam
@@ -1137,7 +1137,15 @@ local rule generate-setup-cmd ( version : command : parent : options * : cpu : g
         }
         else
         {
-            if [ MATCH "(14.3)" : $(version) ]
+            if [ MATCH "(14.4)" : $(version) ]
+            {
+                if $(.debug-configuration)
+                {
+                    ECHO "notice: [generate-setup-cmd] $(version) is 14.4" ;
+                }
+                parent = [ path.native [ path.join  $(parent) "..\\..\\..\\..\\..\\Auxiliary\\Build" ] ] ;
+            }
+            else if [ MATCH "(14.3)" : $(version) ]
             {
                 if $(.debug-configuration)
                 {
@@ -1316,7 +1324,11 @@ local rule configure-really ( version ? : options * )
             # version from the path.
             # FIXME: We currently detect both Microsoft Visual Studio 9.0 and
             # 9.0express as 9.0 here.
-            if [ MATCH "(MSVC\\\\14.3)" : $(command) ]
+            if [ MATCH "(MSVC\\\\14.4)" : $(command) ]
+            {
+                version = 14.4 ;
+            }
+            else if [ MATCH "(MSVC\\\\14.3)" : $(command) ]
             {
                 version = 14.3 ;
             }
@@ -1745,13 +1757,17 @@ local rule default-path ( version )
         # And fortunately, forward slashes do also work in native Windows.
         local vswhere = "$(root)/Microsoft Visual Studio/Installer/vswhere.exe" ;
         # The check for $(root) is to avoid a segmentation fault if not found.
-        if $(version) in 14.1 14.2 14.3 default && $(root) && [ path.exists $(vswhere) ]
+        if $(version) in 14.1 14.2 14.3 14.4 default && $(root) && [ path.exists $(vswhere) ]
         {
             local req = "-requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64" ;
             local prop = "-property installationPath" ;
             local limit ;
 
-            if $(version) = 14.3
+            if $(version) = 14.4
+            {
+                limit = "-version \"[17.0,18.0)\" -prerelease" ;
+            }
+            else if $(version) = 14.3
             {
                 limit = "-version \"[17.0,18.0)\" -prerelease" ;
             }
@@ -2174,7 +2190,7 @@ for local arch in [ MATCH "^\\.cpus-on-(.*)" : [ VARNAMES $(__name__) ] ]
                      armv7 armv7s ;
 
 # Known toolset versions, in order of preference.
-.known-versions = 14.3 14.2 14.1 14.0 12.0 11.0 10.0 10.0express 9.0 9.0express 8.0 8.0express 7.1
+.known-versions = 14.4 14.3 14.2 14.1 14.0 12.0 11.0 10.0 10.0express 9.0 9.0express 8.0 8.0express 7.1
     7.1toolkit 7.0 6.0 ;
 
 # Version aliases.
@@ -2226,6 +2242,11 @@ for local arch in [ MATCH "^\\.cpus-on-(.*)" : [ VARNAMES $(__name__) ] ]
     "Microsoft Visual Studio/2022/*/VC/Tools/MSVC/*/bin/Host*/*"
     ;
 .version-14.3-env = VS170COMNTOOLS ProgramFiles ProgramFiles(x86) ;
+.version-14.4-path =
+    "../../VC/Tools/MSVC/*/bin/Host*/*"
+    "Microsoft Visual Studio/2022/*/VC/Tools/MSVC/*/bin/Host*/*"
+    ;
+.version-14.4-env = VS170COMNTOOLS ProgramFiles ProgramFiles(x86) ;
 
 # Auto-detect all the available msvc installations on the system.
 auto-detect-toolset-versions ;
"""


@versioned
def build_and_install_boost(
    version: str,
    source_dir,
    build_dir,
    install_dir,
    debug: bool,
    cxx: str,
    cflags: List[str],
    cxxflags: List[str],
    linkflags: List[str],
    toolset,
    visibility,
    target_os,
    architecture,
    android_ndk,
    native_api_level,
    address_model="64",
    runtime_link=None,
    android_build_platform="linux-x86_64",
):
    version_underscore = version.replace(".", "_")
    archive = download(
        f"https://archives.boost.io/release/{version}/source/boost_{version_underscore}.tar.gz",
        source_dir,
    )
    extract(archive, output_dir=build_dir, output_dirname="boost")
    with cd(os.path.join(build_dir, "boost")):
        if target_os == "windows":
            bootstrap = ".\\bootstrap.bat"
            b2 = "b2"
        elif target_os == "android" and android_build_platform == "windows-x86_64":
            # Android を Windows でビルドする場合
            bootstrap = ".\\bootstrap.bat"
            b2 = "b2"
        else:
            bootstrap = "./bootstrap.sh"
            b2 = "./b2"

        if runtime_link is None:
            runtime_link = "static" if target_os == "windows" else "shared"

        # Windows かつ Boost 1.85.0 の場合はパッチを当てる
        if target_os == "windows" and version == "1.85.0":
            apply_patch_text(BOOST_PATCH_SUPPORT_14_4, os.path.join(build_dir, "boost"), 1)

        cmd([bootstrap])

        if target_os == "iphone":
            IOS_BUILD_TARGETS = [("arm64", "iphoneos")]
            for arch, sdk in IOS_BUILD_TARGETS:
                clangpp = cmdcap(["xcodebuild", "-find", "clang++"])
                sysroot = cmdcap(["xcrun", "--sdk", sdk, "--show-sdk-path"])
                boost_arch = "x86" if arch == "x86_64" else "arm"
                with open("project-config.jam", "w", encoding="utf-8") as f:
                    f.write(
                        f"using clang \
                        : iphone \
                        : {clangpp} -arch {arch} -isysroot {sysroot} \
                          -fembed-bitcode \
                          -mios-version-min=10.0 \
                          -fvisibility=hidden \
                        : <striper> <root>{sysroot} \
                        ; \
                        "
                    )
                cmd(
                    [
                        b2,
                        "install",
                        "-d+0",
                        f"--build-dir={os.path.join(build_dir, 'boost', f'build-{arch}-{sdk}')}",
                        f"--prefix={os.path.join(build_dir, 'boost', f'install-{arch}-{sdk}')}",
                        "--with-json",
                        "--with-filesystem",
                        "--layout=system",
                        "--ignore-site-config",
                        f"variant={'debug' if debug else 'release'}",
                        f"cflags={' '.join(cflags)}",
                        f"cxxflags={' '.join(cxxflags)}",
                        f"linkflags={' '.join(linkflags)}",
                        f"toolset={toolset}",
                        f"visibility={visibility}",
                        f"target-os={target_os}",
                        f"address-model={address_model}",
                        "link=static",
                        f"runtime-link={runtime_link}",
                        "threading=multi",
                        f"architecture={boost_arch}",
                    ]
                )
            arch, sdk = IOS_BUILD_TARGETS[0]
            installed_path = os.path.join(build_dir, "boost", f"install-{arch}-{sdk}")
            rm_rf(os.path.join(install_dir, "boost"))
            cmd(["cp", "-r", installed_path, os.path.join(install_dir, "boost")])

            for lib in enum_all_files(
                os.path.join(installed_path, "lib"), os.path.join(installed_path, "lib")
            ):
                if not lib.endswith(".a"):
                    continue
                files = [
                    os.path.join(build_dir, "boost", f"install-{arch}-{sdk}", "lib", lib)
                    for arch, sdk in IOS_BUILD_TARGETS
                ]
                if len(files) == 1:
                    shutil.copyfile(files[0], os.path.join(install_dir, "boost", "lib", lib))
                else:
                    cmd(
                        [
                            "lipo",
                            "-create",
                            "-output",
                            os.path.join(install_dir, "boost", "lib", lib),
                        ]
                        + files
                    )
        elif target_os == "android":
            # Android の場合、android-ndk を使ってビルドする
            with open("project-config.jam", "w", encoding="utf-8") as f:
                bin = os.path.join(
                    android_ndk, "toolchains", "llvm", "prebuilt", android_build_platform, "bin"
                )
                sysroot = os.path.join(
                    android_ndk, "toolchains", "llvm", "prebuilt", android_build_platform, "sysroot"
                )

                def escape(s):
                    return s.replace("\\", "/").replace(":", "\\:")

                f.write(
                    f"using clang \
                    : android \
                    : {escape(os.path.join(bin, 'clang++'))} \
                      --target=aarch64-none-linux-android{native_api_level} \
                      --sysroot={escape(sysroot)} \
                    : <archiver>{escape(os.path.join(bin, 'llvm-ar'))} \
                      <ranlib>{escape(os.path.join(bin, 'llvm-ranlib'))} \
                    ; \
                    "
                )
            cmd(
                [
                    b2,
                    "install",
                    "-d+0",
                    f"--prefix={os.path.join(install_dir, 'boost')}",
                    "--with-json",
                    "--with-filesystem",
                    "--layout=system",
                    "--ignore-site-config",
                    f"variant={'debug' if debug else 'release'}",
                    f"compileflags=--sysroot={sysroot}",
                    f"cflags={' '.join(cflags)}",
                    f"cxxflags={' '.join(cxxflags)}",
                    f"linkflags={' '.join(linkflags)}",
                    f"toolset={toolset}",
                    f"visibility={visibility}",
                    f"target-os={target_os}",
                    f"address-model={address_model}",
                    "link=static",
                    f"runtime-link={runtime_link}",
                    "threading=multi",
                    "architecture=arm",
                ]
            )
        else:
            if len(cxx) != 0:
                with open("project-config.jam", "w", encoding="utf-8") as f:
                    f.write(f"using {toolset} : : {cxx} : ;")
            cmd(
                [
                    b2,
                    "install",
                    "-d+0",
                    f"--prefix={os.path.join(install_dir, 'boost')}",
                    "--with-json",
                    "--with-filesystem",
                    "--layout=system",
                    "--ignore-site-config",
                    f"variant={'debug' if debug else 'release'}",
                    f"cflags={' '.join(cflags)}",
                    f"cxxflags={' '.join(cxxflags)}",
                    f"linkflags={' '.join(linkflags)}",
                    f"toolset={toolset}",
                    f"visibility={visibility}",
                    f"target-os={target_os}",
                    f"address-model={address_model}",
                    "link=static",
                    f"runtime-link={runtime_link}",
                    "threading=multi",
                    f"architecture={architecture}",
                ]
            )


@versioned
def install_sora(version, source_dir, install_dir, platform: str):
    win = platform.startswith("windows_")
    filename = f"sora-cpp-sdk-{version}_{platform}.{'zip' if win else 'tar.gz'}"
    rm_rf(os.path.join(source_dir, filename))
    archive = download(
        f"https://github.com/shiguredo/sora-cpp-sdk/releases/download/{version}/{filename}",
        output_dir=source_dir,
    )
    rm_rf(os.path.join(install_dir, "sora"))
    extract(archive, output_dir=install_dir, output_dirname="sora")


def install_sora_and_deps(
    sora_version: str, boost_version: str, platform: str, source_dir: str, install_dir: str
):
    # Boost
    install_boost_args = {
        "version": boost_version,
        "version_file": os.path.join(install_dir, "boost.version"),
        "source_dir": source_dir,
        "install_dir": install_dir,
        "sora_version": sora_version,
        "platform": platform,
    }
    install_boost(**install_boost_args)

    # Sora C++ SDK
    install_sora_args = {
        "version": sora_version,
        "version_file": os.path.join(install_dir, "sora.version"),
        "source_dir": source_dir,
        "install_dir": install_dir,
        "platform": platform,
    }
    install_sora(**install_sora_args)


def build_sora(
    platform: str,
    local_sora_cpp_sdk_dir: str,
    local_sora_cpp_sdk_args: List[str],
    debug: bool,
    local_webrtc_build_dir: Optional[str],
):
    if debug and "--debug" not in local_sora_cpp_sdk_args:
        local_sora_cpp_sdk_args = ["--debug", *local_sora_cpp_sdk_args]
    if local_webrtc_build_dir is not None:
        local_sora_cpp_sdk_args = [
            "--local-webrtc-build-dir",
            local_webrtc_build_dir,
            *local_sora_cpp_sdk_args,
        ]

    with cd(local_sora_cpp_sdk_dir):
        cmd(["python3", "run.py", platform, *local_sora_cpp_sdk_args])


class SoraInfo(NamedTuple):
    sora_install_dir: str
    boost_install_dir: str


def get_sora_info(
    platform: str, local_sora_cpp_sdk_dir: Optional[str], install_dir: str, debug: bool
) -> SoraInfo:
    if local_sora_cpp_sdk_dir is not None:
        configuration = "debug" if debug else "release"
        install_dir = os.path.join(local_sora_cpp_sdk_dir, "_install", platform, configuration)

    return SoraInfo(
        sora_install_dir=os.path.join(install_dir, "sora"),
        boost_install_dir=os.path.join(install_dir, "boost"),
    )


@versioned
def install_rootfs(version, install_dir, conf, arch="arm64"):
    rootfs_dir = os.path.join(install_dir, "rootfs")
    rm_rf(rootfs_dir)
    cmd(["multistrap", "--no-auth", "-a", arch, "-d", rootfs_dir, "-f", conf])
    # 絶対パスのシンボリックリンクを相対パスに置き換えていく
    for dir, _, filenames in os.walk(rootfs_dir):
        for filename in filenames:
            linkpath = os.path.join(dir, filename)
            # symlink かどうか
            if not os.path.islink(linkpath):
                continue
            target = os.readlink(linkpath)
            # 絶対パスかどうか
            if not os.path.isabs(target):
                continue
            # rootfs_dir を先頭に付けることで、
            # rootfs の外から見て正しい絶対パスにする
            targetpath = rootfs_dir + target
            # 参照先の絶対パスが存在するかどうか
            if not os.path.exists(targetpath):
                continue
            # 相対パスに置き換える
            relpath = os.path.relpath(targetpath, dir)
            logging.debug(f"{linkpath[len(rootfs_dir) :]} targets {target} to {relpath}")
            os.remove(linkpath)
            os.symlink(relpath, linkpath)

    # なぜかシンボリックリンクが登録されていないので作っておく
    link = os.path.join(rootfs_dir, "usr", "lib", "aarch64-linux-gnu", "tegra", "libnvbuf_fdmap.so")
    file = os.path.join(
        rootfs_dir, "usr", "lib", "aarch64-linux-gnu", "tegra", "libnvbuf_fdmap.so.1.0.0"
    )
    if os.path.exists(file) and not os.path.exists(link):
        os.symlink(os.path.basename(file), link)

    # JetPack 6 から tegra → nvidia になった
    link = os.path.join(
        rootfs_dir, "usr", "lib", "aarch64-linux-gnu", "nvidia", "libnvbuf_fdmap.so"
    )
    file = os.path.join(
        rootfs_dir, "usr", "lib", "aarch64-linux-gnu", "nvidia", "libnvbuf_fdmap.so.1.0.0"
    )
    if os.path.exists(file) and not os.path.exists(link):
        os.symlink(os.path.basename(file), link)


@versioned
def install_android_ndk(version, install_dir, source_dir, platform="linux"):
    if platform not in ("windows", "darwin", "linux"):
        raise Exception(f"Not supported platform: {platform}")

    if platform == "windows":
        url = f"https://dl.google.com/android/repository/android-ndk-{version}-{platform}.zip"
    elif platform == "darwin":
        url = f"https://dl.google.com/android/repository/android-ndk-{version}-{platform}.dmg"
    else:
        url = f"https://dl.google.com/android/repository/android-ndk-{version}-{platform}.zip"
    archive = download(url, source_dir)
    rm_rf(os.path.join(install_dir, "android-ndk"))
    if platform == "darwin":
        cap = cmdcap(["hdiutil", "attach", archive])
        # 以下のような結果が得られるはずなので、ここから /Volumes/Android NDK r26 のところだけ取り出す
        # /dev/disk4              GUID_partition_scheme
        # /dev/disk4s1            EFI
        # /dev/disk4s2            Apple_HFS                       /Volumes/Android NDK r26
        volume = cap.split("\n")[-1].split("\t")[-1]
        # AndroidNDK10792818.app みたいな感じの app があるはず
        app = glob.glob("AndroidNDK*.app", root_dir=volume)[0]
        # NDK ディレクトリをコピー
        cmd(
            [
                "cp",
                "-r",
                os.path.join(volume, app, "Contents", "NDK"),
                os.path.join(install_dir, "android-ndk"),
            ]
        )
        cmdcap(["hdiutil", "detach", volume])
    else:
        extract(archive, output_dir=install_dir, output_dirname="android-ndk")


@versioned
def install_android_sdk_cmdline_tools(version, install_dir, source_dir):
    archive = download(
        f"https://dl.google.com/android/repository/commandlinetools-linux-{version}_latest.zip",
        source_dir,
    )
    tools_dir = os.path.join(install_dir, "android-sdk-cmdline-tools")
    rm_rf(tools_dir)
    extract(archive, output_dir=tools_dir, output_dirname="cmdline-tools")
    sdkmanager = os.path.join(tools_dir, "cmdline-tools", "bin", "sdkmanager")
    # ライセンスを許諾する
    cmd(["/bin/bash", "-c", f"yes | {sdkmanager} --sdk_root={tools_dir} --licenses"])


@versioned
def install_llvm(
    version,
    install_dir,
    tools_url,
    tools_commit,
    libcxx_url,
    libcxx_commit,
    buildtools_url,
    buildtools_commit,
):
    llvm_dir = os.path.join(install_dir, "llvm")
    rm_rf(llvm_dir)
    mkdir_p(llvm_dir)
    with cd(llvm_dir):
        # tools の update.py を叩いて特定バージョンの clang バイナリを拾う
        git_clone_shallow(tools_url, tools_commit, "tools")
        with cd("tools"):
            cmd(
                [
                    "python3",
                    os.path.join("clang", "scripts", "update.py"),
                    "--output-dir",
                    os.path.join(llvm_dir, "clang"),
                ]
            )

        # 特定バージョンの libcxx を利用する
        git_clone_shallow(libcxx_url, libcxx_commit, "libcxx")

        # __config_site のために特定バージョンの buildtools を取得する
        git_clone_shallow(buildtools_url, buildtools_commit, "buildtools")
        with cd("buildtools"):
            cmd(["git", "reset", "--hard", buildtools_commit])
        shutil.copyfile(
            os.path.join(llvm_dir, "buildtools", "third_party", "libc++", "__config_site"),
            os.path.join(llvm_dir, "libcxx", "include", "__config_site"),
        )

        # __assertion_handler をコピーする
        # 背景: https://source.chromium.org/chromium/_/chromium/external/github.com/llvm/llvm-project/libcxx.git/+/1e5bda0d1ce8e346955aa4a85eaab258785f11f7
        shutil.copyfile(
            # NOTE(enm10k): 最初は default_assertion_handler.in をコピーしていたが、 buildtools 以下に
            # default_assertion_handler.in から生成されたと思われる __assertion_handler が存在するため、それをコピーする
            # os.path.join(llvm_dir, "libcxx", "vendor", "llvm", "default_assertion_handler.in"),
            os.path.join(llvm_dir, "buildtools", "third_party", "libc++", "__assertion_handler"),
            os.path.join(llvm_dir, "libcxx", "include", "__assertion_handler"),
        )


def cmake_path(path: str) -> str:
    return path.replace("\\", "/")


@versioned
def install_cmake(version, source_dir, install_dir, platform: str, ext):
    url = f"https://github.com/Kitware/CMake/releases/download/v{version}/cmake-{version}-{platform}.{ext}"
    path = download(url, source_dir)
    extract(path, install_dir, "cmake")
    # Android で自前の CMake を利用する場合、ninja へのパスが見つけられない問題があるので、同じディレクトリに symlink を貼る
    # https://issuetracker.google.com/issues/206099937
    if platform.startswith("linux"):
        with cd(os.path.join(install_dir, "cmake", "bin")):
            cmd(["ln", "-s", "/usr/bin/ninja", "ninja"])


@versioned
def install_sdl2(
    version, source_dir, build_dir, install_dir, debug: bool, platform: str, cmake_args: List[str]
):
    url = f"http://www.libsdl.org/release/SDL2-{version}.zip"
    path = download(url, source_dir)
    sdl2_source_dir = os.path.join(source_dir, "sdl2")
    sdl2_build_dir = os.path.join(build_dir, "sdl2")
    sdl2_install_dir = os.path.join(install_dir, "sdl2")
    rm_rf(sdl2_source_dir)
    rm_rf(sdl2_build_dir)
    rm_rf(sdl2_install_dir)
    extract(path, source_dir, "sdl2")

    mkdir_p(sdl2_build_dir)
    with cd(sdl2_build_dir):
        configuration = "Debug" if debug else "Release"
        cmake_args = cmake_args[:]
        cmake_args += [
            sdl2_source_dir,
            f"-DCMAKE_BUILD_TYPE={configuration}",
            f"-DCMAKE_INSTALL_PREFIX={cmake_path(sdl2_install_dir)}",
            "-DBUILD_SHARED_LIBS=OFF",
        ]
        if platform == "windows":
            cmake_args += [
                "-DSDL_FORCE_STATIC_VCRT=ON",
                "-DHAVE_LIBC=ON",
            ]
        elif platform == "macos":
            # システムでインストール済みかによって ON/OFF が切り替わってしまうため、
            # どの環境でも同じようにインストールされるようにするため全部 ON/OFF を明示的に指定する
            cmake_args += [
                "-DSDL_ATOMIC=OFF",
                "-DSDL_AUDIO=OFF",
                "-DSDL_VIDEO=ON",
                "-DSDL_RENDER=ON",
                "-DSDL_EVENTS=ON",
                "-DSDL_JOYSTICK=ON",
                "-DSDL_HAPTIC=ON",
                "-DSDL_POWER=ON",
                "-DSDL_THREADS=ON",
                "-DSDL_TIMERS=OFF",
                "-DSDL_FILE=OFF",
                "-DSDL_LOADSO=ON",
                "-DSDL_CPUINFO=OFF",
                "-DSDL_FILESYSTEM=OFF",
                "-DSDL_SENSOR=ON",
                "-DSDL_OPENGL=ON",
                "-DSDL_OPENGLES=ON",
                "-DSDL_RPI=OFF",
                "-DSDL_WAYLAND=OFF",
                "-DSDL_X11=OFF",
                "-DSDL_VULKAN=OFF",
                "-DSDL_VIVANTE=OFF",
                "-DSDL_COCOA=ON",
                "-DSDL_METAL=ON",
                "-DSDL_KMSDRM=OFF",
            ]
        elif platform == "linux":
            # システムでインストール済みかによって ON/OFF が切り替わってしまうため、
            # どの環境でも同じようにインストールされるようにするため全部 ON/OFF を明示的に指定する
            cmake_args += [
                "-DSDL_ATOMIC=OFF",
                "-DSDL_AUDIO=OFF",
                "-DSDL_VIDEO=ON",
                "-DSDL_RENDER=ON",
                "-DSDL_EVENTS=ON",
                "-DSDL_JOYSTICK=ON",
                "-DSDL_HAPTIC=ON",
                "-DSDL_POWER=ON",
                "-DSDL_THREADS=ON",
                "-DSDL_TIMERS=OFF",
                "-DSDL_FILE=OFF",
                "-DSDL_LOADSO=ON",
                "-DSDL_CPUINFO=OFF",
                "-DSDL_FILESYSTEM=OFF",
                "-DSDL_SENSOR=ON",
                "-DSDL_OPENGL=ON",
                "-DSDL_OPENGLES=ON",
                "-DSDL_RPI=OFF",
                "-DSDL_WAYLAND=OFF",
                "-DSDL_X11=ON",
                "-DSDL_X11_SHARED=OFF",
                "-DSDL_X11_XCURSOR=OFF",
                "-DSDL_X11_XDBE=OFF",
                "-DSDL_X11_XFIXES=OFF",
                "-DSDL_X11_XINERAMA=OFF",
                "-DSDL_X11_XINPUT=OFF",
                "-DSDL_X11_XRANDR=OFF",
                "-DSDL_X11_XSCRNSAVER=OFF",
                "-DSDL_X11_XSHAPE=OFF",
                "-DSDL_X11_XVM=OFF",
                "-DSDL_VULKAN=OFF",
                "-DSDL_VIVANTE=OFF",
                "-DSDL_COCOA=OFF",
                "-DSDL_METAL=OFF",
                "-DSDL_KMSDRM=OFF",
            ]
        cmd(["cmake"] + cmake_args)

        cmd(
            ["cmake", "--build", ".", "--config", configuration, f"-j{multiprocessing.cpu_count()}"]
        )
        cmd(["cmake", "--install", ".", "--config", configuration])


@versioned
def install_cli11(version, install_dir):
    cli11_install_dir = os.path.join(install_dir, "cli11")
    rm_rf(cli11_install_dir)
    cmd(
        [
            "git",
            "clone",
            "--branch",
            version,
            "--depth",
            "1",
            "https://github.com/CLIUtils/CLI11.git",
            cli11_install_dir,
        ]
    )


@versioned
def install_cuda_windows(version, source_dir, build_dir, install_dir):
    rm_rf(os.path.join(build_dir, "cuda"))
    rm_rf(os.path.join(install_dir, "cuda"))
    if version == "10.2.89-1":
        url = "http://developer.download.nvidia.com/compute/cuda/10.2/Prod/local_installers/cuda_10.2.89_441.22_win10.exe"  # noqa: E501
    elif version == "11.8.0-1":
        url = "https://developer.download.nvidia.com/compute/cuda/11.8.0/local_installers/cuda_11.8.0_522.06_windows.exe"  # noqa: E501
    else:
        raise Exception(f"Unknown CUDA version {version}")
    file = download(url, source_dir)

    mkdir_p(os.path.join(build_dir, "cuda"))
    mkdir_p(os.path.join(install_dir, "cuda"))
    with cd(os.path.join(build_dir, "cuda")):
        cmd(["7z", "x", file])
    copytree(
        os.path.join(build_dir, "cuda", "cuda_nvcc", "nvcc"), os.path.join(install_dir, "cuda")
    )
    copytree(
        os.path.join(build_dir, "cuda", "cuda_cudart", "cudart"), os.path.join(install_dir, "cuda")
    )


@versioned
def install_vpl(version, configuration, source_dir, build_dir, install_dir, cmake_args):
    vpl_source_dir = os.path.join(source_dir, "vpl")
    vpl_build_dir = os.path.join(build_dir, "vpl")
    vpl_install_dir = os.path.join(install_dir, "vpl")
    rm_rf(vpl_source_dir)
    rm_rf(vpl_build_dir)
    rm_rf(vpl_install_dir)
    git_clone_shallow("https://github.com/intel/libvpl.git", version, vpl_source_dir)

    mkdir_p(vpl_build_dir)
    with cd(vpl_build_dir):
        cmd(
            [
                "cmake",
                f"-DCMAKE_INSTALL_PREFIX={cmake_path(vpl_install_dir)}",
                f"-DCMAKE_BUILD_TYPE={configuration}",
                "-DBUILD_SHARED_LIBS=OFF",
                "-DBUILD_TOOLS=OFF",
                "-DBUILD_EXAMPLES=OFF",
                "-DBUILD_PREVIEW=OFF",
                "-DINSTALL_EXAMPLE_CODE=OFF",
                "-DBUILD_TOOLS_ONEVPL_EXPERIMENTAL=OFF",
                "-DUSE_MSVC_STATIC_RUNTIME=ON",
                vpl_source_dir,
                *cmake_args,
            ]
        )
        # 生成されたプロジェクトに対して静的ランタイムを使うように変更する
        vpl_path = os.path.join("libvpl", "VPL.vcxproj")
        if os.path.exists(vpl_path):
            replace_vcproj_static_runtime(vpl_path)

        cmd(
            ["cmake", "--build", ".", f"-j{multiprocessing.cpu_count()}", "--config", configuration]
        )
        cmd(["cmake", "--install", ".", "--config", configuration])


@versioned
def install_blend2d(
    version,
    configuration,
    source_dir,
    build_dir,
    install_dir,
    blend2d_version,
    asmjit_version,
    ios,
    cmake_args,
):
    rm_rf(os.path.join(source_dir, "blend2d"))
    rm_rf(os.path.join(build_dir, "blend2d"))
    rm_rf(os.path.join(install_dir, "blend2d"))

    git_clone_shallow(
        "https://github.com/blend2d/blend2d", blend2d_version, os.path.join(source_dir, "blend2d")
    )
    mkdir_p(os.path.join(source_dir, "blend2d", "3rdparty"))
    git_clone_shallow(
        "https://github.com/asmjit/asmjit",
        asmjit_version,
        os.path.join(source_dir, "blend2d", "3rdparty", "asmjit"),
    )

    mkdir_p(os.path.join(build_dir, "blend2d"))
    with cd(os.path.join(build_dir, "blend2d")):
        cmd(
            [
                "cmake",
                os.path.join(source_dir, "blend2d"),
                f"-DCMAKE_BUILD_TYPE={configuration}",
                f"-DCMAKE_INSTALL_PREFIX={cmake_path(os.path.join(install_dir, 'blend2d'))}",
                "-DBLEND2D_STATIC=ON",
                *cmake_args,
            ]
        )
        # 生成されたプロジェクトに対して静的ランタイムを使うように変更する
        project_path = os.path.join(build_dir, "blend2d", "blend2d.vcxproj")
        if os.path.exists(project_path):
            replace_vcproj_static_runtime(project_path)

        if ios:
            cmd(
                [
                    "cmake",
                    "--build",
                    ".",
                    f"-j{multiprocessing.cpu_count()}",
                    "--config",
                    configuration,
                    "--target",
                    "blend2d",
                    "--",
                    "-arch",
                    "arm64",
                    "-sdk",
                    "iphoneos",
                ]
            )
            cmd(["cmake", "--build", ".", "--target", "install", "--config", configuration])
        else:
            cmd(
                [
                    "cmake",
                    "--build",
                    ".",
                    f"-j{multiprocessing.cpu_count()}",
                    "--config",
                    configuration,
                ]
            )
            cmd(["cmake", "--build", ".", "--target", "install", "--config", configuration])


@versioned
def install_openh264(version, source_dir, install_dir, is_windows):
    rm_rf(os.path.join(source_dir, "openh264"))
    rm_rf(os.path.join(install_dir, "openh264"))
    git_clone_shallow(
        "https://github.com/cisco/openh264.git", version, os.path.join(source_dir, "openh264")
    )
    with cd(os.path.join(source_dir, "openh264")):
        if is_windows:
            # Windows は make が無いので手動でコピーする
            # install-headers:
            # 	mkdir -p $(DESTDIR)$(PREFIX)/include/wels
            # 	install -m 644 $(SRC_PATH)/codec/api/wels/codec*.h $(DESTDIR)$(PREFIX)/include/wels
            mkdir_p(os.path.join(install_dir, "openh264", "include", "wels"))
            with cd(os.path.join("codec", "api", "wels")):
                for file in glob.glob("codec*.h"):
                    shutil.copyfile(
                        file, os.path.join(install_dir, "openh264", "include", "wels", file)
                    )
        else:
            cmd(["make", f"PREFIX={os.path.join(install_dir, 'openh264')}", "install-headers"])


@versioned
def install_yaml(version, source_dir, build_dir, install_dir, cmake_args):
    rm_rf(os.path.join(source_dir, "yaml"))
    rm_rf(os.path.join(install_dir, "yaml"))
    rm_rf(os.path.join(build_dir, "yaml"))
    git_clone_shallow(
        "https://github.com/jbeder/yaml-cpp.git", version, os.path.join(source_dir, "yaml")
    )

    mkdir_p(os.path.join(build_dir, "yaml"))
    with cd(os.path.join(build_dir, "yaml")):
        cmd(
            [
                "cmake",
                os.path.join(source_dir, "yaml"),
                "-DCMAKE_BUILD_TYPE=Release",
                f"-DCMAKE_INSTALL_PREFIX={install_dir}/yaml",
                "-DYAML_CPP_BUILD_TESTS=OFF",
                "-DYAML_CPP_BUILD_TOOLS=OFF",
                *cmake_args,
            ]
        )
        cmd(["cmake", "--build", ".", f"-j{multiprocessing.cpu_count()}"])
        cmd(["cmake", "--build", ".", "--target", "install"])


@versioned
def install_catch2(version, source_dir, build_dir, install_dir, configuration, cmake_args):
    rm_rf(os.path.join(source_dir, "catch2"))
    rm_rf(os.path.join(install_dir, "catch2"))
    rm_rf(os.path.join(build_dir, "catch2"))
    git_clone_shallow(
        "https://github.com/catchorg/Catch2.git", version, os.path.join(source_dir, "catch2")
    )

    mkdir_p(os.path.join(build_dir, "catch2"))
    with cd(os.path.join(build_dir, "catch2")):
        cmd(
            [
                "cmake",
                os.path.join(source_dir, "catch2"),
                f"-DCMAKE_BUILD_TYPE={configuration}",
                f"-DCMAKE_INSTALL_PREFIX={install_dir}/catch2",
                "-DCATCH_BUILD_TESTING=OFF",
                *cmake_args,
            ]
        )
        # 生成されたプロジェクトに対して静的ランタイムを使うように変更する
        project_path = os.path.join("src", "Catch2.vcxproj")
        if os.path.exists(project_path):
            replace_vcproj_static_runtime(project_path)
        project_path = os.path.join("src", "Catch2WithMain.vcxproj")
        if os.path.exists(project_path):
            replace_vcproj_static_runtime(project_path)
        cmd(
            ["cmake", "--build", ".", "--config", configuration, f"-j{multiprocessing.cpu_count()}"]
        )
        cmd(["cmake", "--build", ".", "--config", configuration, "--target", "install"])


@versioned
def install_protobuf(version, source_dir, install_dir, platform: str):
    # platform:
    # - linux-aarch_64
    # - linux-ppcle_64
    # - linux-s390_64
    # - linux-x86_32
    # - linux-x86_64
    # - osx-aarch_64
    # - osx-universal_binary
    # - osx-x86_64
    # - win32
    # - win64
    url = f"https://github.com/protocolbuffers/protobuf/releases/download/v{version}/protoc-{version}-{platform}.zip"
    path = download(url, source_dir)
    rm_rf(os.path.join(install_dir, "protobuf"))
    extract(path, install_dir, "protobuf")
    # なぜか実行属性が消えてるので入れてやる
    for file in os.scandir(os.path.join(install_dir, "protobuf", "bin")):
        if file.is_file():
            os.chmod(file.path, file.stat().st_mode | stat.S_IXUSR)


@versioned
def install_protoc_gen_jsonif(version, source_dir, install_dir, platform: str):
    # platform:
    # - darwin-amd64
    # - darwin-arm64
    # - linux-amd64
    # - windows-amd64
    url = f"https://github.com/melpon/protoc-gen-jsonif/releases/download/{version}/protoc-gen-jsonif.tar.gz"
    rm_rf(os.path.join(source_dir, "protoc-gen-jsonif.tar.gz"))
    path = download(url, source_dir)
    jsonif_install_dir = os.path.join(install_dir, "protoc-gen-jsonif")
    rm_rf(jsonif_install_dir)
    extract(path, install_dir, "protoc-gen-jsonif")
    # 自分の環境のバイナリを <install-path>/bin に配置する
    shutil.copytree(
        os.path.join(jsonif_install_dir, *platform.split("-")),
        os.path.join(jsonif_install_dir, "bin"),
    )
    # なぜか実行属性が消えてるので入れてやる
    for file in os.scandir(os.path.join(jsonif_install_dir, "bin")):
        if file.is_file():
            os.chmod(file.path, file.stat().st_mode | stat.S_IXUSR)


# iOS, Android などのクロスコンパイル環境で実行可能ファイルを生成しようとしてエラーになるのを防止するパッチ
#
# v1.64.1 をベースにパッチを当てている
#
# MEMO: gRPC の、submodule を含めて全ての diff を取得するコマンド
#   git --no-pager diff --ignore-submodules && git submodule foreach --recursive 'git --no-pager diff --ignore-submodules --src-prefix a/$path/ --dst-prefix b/$path/' | grep -v '^Entering'
GRPC_PATCH_NO_EXECUTABLE = r"""
diff --git a/third_party/boringssl-with-bazel/CMakeLists.txt b/third_party/boringssl-with-bazel/CMakeLists.txt
index 6464e200f..c7bc417a1 100644
--- a/third_party/boringssl-with-bazel/CMakeLists.txt
+++ b/third_party/boringssl-with-bazel/CMakeLists.txt
@@ -543,30 +543,6 @@ add_library(
 
 target_link_libraries(ssl crypto)
 
-add_executable(
-  bssl
-
-  src/tool/args.cc
-  src/tool/ciphers.cc
-  src/tool/client.cc
-  src/tool/const.cc
-  src/tool/digest.cc
-  src/tool/fd.cc
-  src/tool/file.cc
-  src/tool/generate_ech.cc
-  src/tool/generate_ed25519.cc
-  src/tool/genrsa.cc
-  src/tool/pkcs12.cc
-  src/tool/rand.cc
-  src/tool/server.cc
-  src/tool/sign.cc
-  src/tool/speed.cc
-  src/tool/tool.cc
-  src/tool/transport_common.cc
-)
-
-target_link_libraries(bssl ssl crypto)
-
 if(NOT CMAKE_SYSTEM_NAME STREQUAL "Android")
   find_package(Threads REQUIRED)
   target_link_libraries(crypto Threads::Threads)
diff --git a/third_party/zlib/CMakeLists.txt b/third_party/zlib/CMakeLists.txt
index 7f1b69f..bcf5577 100644
--- a/third_party/zlib/CMakeLists.txt
+++ b/third_party/zlib/CMakeLists.txt
@@ -147,10 +147,7 @@ if(MINGW)
     set(ZLIB_DLL_SRCS ${CMAKE_CURRENT_BINARY_DIR}/zlib1rc.obj)
 endif(MINGW)
 
-add_library(zlib SHARED ${ZLIB_SRCS} ${ZLIB_DLL_SRCS} ${ZLIB_PUBLIC_HDRS} ${ZLIB_PRIVATE_HDRS})
 add_library(zlibstatic STATIC ${ZLIB_SRCS} ${ZLIB_PUBLIC_HDRS} ${ZLIB_PRIVATE_HDRS})
-set_target_properties(zlib PROPERTIES DEFINE_SYMBOL ZLIB_DLL)
-set_target_properties(zlib PROPERTIES SOVERSION 1)
 
 if(NOT CYGWIN)
     # This property causes shared libraries on Linux to have the full version
@@ -160,22 +157,16 @@ if(NOT CYGWIN)
     #
     # This has no effect with MSVC, on that platform the version info for
     # the DLL comes from the resource file win32/zlib1.rc
-    set_target_properties(zlib PROPERTIES VERSION ${ZLIB_FULL_VERSION})
 endif()
 
 if(UNIX)
     # On unix-like platforms the library is almost always called libz
-   set_target_properties(zlib zlibstatic PROPERTIES OUTPUT_NAME z)
-   if(NOT APPLE)
-     set_target_properties(zlib PROPERTIES LINK_FLAGS "-Wl,--version-script,\"${CMAKE_CURRENT_SOURCE_DIR}/zlib.map\"")
-   endif()
 elseif(BUILD_SHARED_LIBS AND WIN32)
     # Creates zlib1.dll when building shared library version
-    set_target_properties(zlib PROPERTIES SUFFIX "1.dll")
 endif()
 
 if(NOT SKIP_INSTALL_LIBRARIES AND NOT SKIP_INSTALL_ALL )
-    install(TARGETS zlib zlibstatic
+    install(TARGETS zlibstatic
         RUNTIME DESTINATION "${INSTALL_BIN_DIR}"
         ARCHIVE DESTINATION "${INSTALL_LIB_DIR}"
         LIBRARY DESTINATION "${INSTALL_LIB_DIR}" )
@@ -193,21 +184,3 @@ endif()
 #============================================================================
 # Example binaries
 #============================================================================
-
-add_executable(example test/example.c)
-target_link_libraries(example zlib)
-add_test(example example)
-
-add_executable(minigzip test/minigzip.c)
-target_link_libraries(minigzip zlib)
-
-if(HAVE_OFF64_T)
-    add_executable(example64 test/example.c)
-    target_link_libraries(example64 zlib)
-    set_target_properties(example64 PROPERTIES COMPILE_FLAGS "-D_FILE_OFFSET_BITS=64")
-    add_test(example64 example64)
-
-    add_executable(minigzip64 test/minigzip.c)
-    target_link_libraries(minigzip64 zlib)
-    set_target_properties(minigzip64 PROPERTIES COMPILE_FLAGS "-D_FILE_OFFSET_BITS=64")
-endif()
"""


@versioned
def install_grpc(
    version,
    source_dir,
    build_dir,
    install_dir,
    debug: bool,
    cmake_args: List[str],
    cmake_build_args: List[str] = [],
):
    grpc_source_dir = os.path.join(source_dir, "grpc")
    grpc_build_dir = os.path.join(build_dir, "grpc")
    grpc_install_dir = os.path.join(install_dir, "grpc")
    rm_rf(grpc_source_dir)
    rm_rf(grpc_build_dir)
    rm_rf(grpc_install_dir)
    git_clone_shallow("https://github.com/grpc/grpc.git", version, grpc_source_dir, submodule=True)
    apply_patch_text(GRPC_PATCH_NO_EXECUTABLE, grpc_source_dir, 1)
    mkdir_p(grpc_build_dir)
    with cd(grpc_build_dir):
        configuration = "Debug" if debug else "Release"
        cmd(
            [
                "cmake",
                grpc_source_dir,
                f"-DCMAKE_INSTALL_PREFIX={cmake_path(grpc_install_dir)}",
                f"-DCMAKE_BUILD_TYPE={configuration}",
                *cmake_args,
            ]
        )
        cmd(
            [
                "cmake",
                "--build",
                ".",
                f"-j{multiprocessing.cpu_count()}",
                "--config",
                configuration,
                *cmake_build_args,
            ]
        )
        cmd(["cmake", "--install", ".", "--config", configuration])


@versioned
def install_ggrpc(version, install_dir):
    ggrpc_install_dir = os.path.join(install_dir, "ggrpc")
    rm_rf(ggrpc_install_dir)
    git_clone_shallow("https://github.com/melpon/ggrpc.git", version, ggrpc_install_dir)


@versioned
def install_spdlog(version, install_dir):
    spdlog_install_dir = os.path.join(install_dir, "spdlog")
    rm_rf(spdlog_install_dir)
    git_clone_shallow("https://github.com/gabime/spdlog.git", version, spdlog_install_dir)


BORINGSSL_PATCH_NO_BSSL = r"""
diff --git a/CMakeLists.txt b/CMakeLists.txt
index 38d63db..b97b175 100644
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -795,7 +795,7 @@ endif()
 
 if(INSTALL_ENABLED)
   install(TARGETS crypto ssl EXPORT OpenSSLTargets)
-  install(TARGETS bssl)
+  # install(TARGETS bssl)
   install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
   install(EXPORT OpenSSLTargets
           FILE OpenSSLTargets.cmake
"""


@versioned
def install_boringssl(
    version,
    source_dir: str,
    build_dir: str,
    install_dir: str,
    configuration: str,
    cmake_args: List[str],
):
    boringssl_source_dir = os.path.join(source_dir, "boringssl")
    boringssl_build_dir = os.path.join(build_dir, "boringssl")
    boringssl_install_dir = os.path.join(install_dir, "boringssl")
    rm_rf(boringssl_source_dir)
    rm_rf(boringssl_build_dir)
    rm_rf(boringssl_install_dir)
    git_clone_shallow("https://boringssl.googlesource.com/boringssl", version, boringssl_source_dir)
    apply_patch_text(BORINGSSL_PATCH_NO_BSSL, boringssl_source_dir, 1)
    mkdir_p(boringssl_build_dir)
    with cd(boringssl_build_dir):
        cmd(
            [
                "cmake",
                f"-DCMAKE_INSTALL_PREFIX={cmake_path(boringssl_install_dir)}",
                f"-DCMAKE_BUILD_TYPE={configuration}",
                "-DBUILD_SHARED_LIBS=OFF",
                boringssl_source_dir,
                *cmake_args,
            ]
        )
        cmd(
            ["cmake", "--build", ".", f"-j{multiprocessing.cpu_count()}", "--config", configuration]
        )
        cmd(["cmake", "--install", ".", "--config", configuration])


@versioned
def install_opus(
    version, source_dir, build_dir, install_dir, configuration: str, cmake_args: List[str]
):
    opus_source_dir = os.path.join(source_dir, "opus")
    opus_build_dir = os.path.join(build_dir, "opus")
    opus_install_dir = os.path.join(install_dir, "opus")
    rm_rf(opus_source_dir)
    rm_rf(opus_build_dir)
    rm_rf(opus_install_dir)
    git_clone_shallow("https://gitlab.xiph.org/xiph/opus", version, opus_source_dir)
    mkdir_p(opus_build_dir)
    with cd(opus_build_dir):
        cmd(
            [
                "cmake",
                f"-DCMAKE_INSTALL_PREFIX={cmake_path(opus_install_dir)}",
                f"-DCMAKE_BUILD_TYPE={configuration}",
                "-DOPUS_BUILD_SHARED_LIBRARY=OFF",
                "-DOPUS_BUILD_TESTING=OFF",
                "-DOPUS_BUILD_PROGRAMS=OFF",
                "-DOPUS_STATIC_RUNTIME=ON",
                opus_source_dir,
                *cmake_args,
            ]
        )
        cmd(
            ["cmake", "--build", ".", f"-j{multiprocessing.cpu_count()}", "--config", configuration]
        )
        cmd(["cmake", "--install", ".", "--config", configuration])


@versioned
def install_nasm(version, source_dir, install_dir, platform: str):
    if platform not in ("macosx", "win32", "win64"):
        raise Exception(f"Unsupported platform: {platform}")
    url = f"https://www.nasm.us/pub/nasm/releasebuilds/{version}/{platform}/nasm-{version}-{platform}.zip"
    path = download(url, source_dir)
    nasm_install_dir = os.path.join(install_dir, "nasm")
    rm_rf(nasm_install_dir)
    extract(path, install_dir, "nasm")


@versioned
def install_ninja(version, source_dir, install_dir, platform):
    if platform not in ("win", "winarm64", "linux-aarch64", "linux", "mac"):
        raise Exception(f"Unsupported platform: {platform}")
    url = f"https://github.com/ninja-build/ninja/releases/download/{version}/ninja-{platform}.zip"
    path = download(url, source_dir)
    ninja_install_dir = os.path.join(install_dir, "ninja")
    rm_rf(ninja_install_dir)
    extract(path, install_dir, "ninja")


@versioned
def install_vswhere(version, install_dir):
    url = f"https://github.com/microsoft/vswhere/releases/download/{version}/vswhere.exe"
    vswhere_install_dir = os.path.join(install_dir, "vswhere")
    rm_rf(vswhere_install_dir)
    mkdir_p(vswhere_install_dir)
    download(url, vswhere_install_dir)


class PlatformTarget(object):
    def __init__(self, os, osver, arch, extra=None):
        self.os = os
        self.osver = osver
        self.arch = arch
        self.extra = extra

    @property
    def package_name(self):
        if self.os == "windows":
            return f"windows_{self.arch}"
        if self.os == "macos":
            return f"macos_{self.arch}"
        if self.os == "ubuntu":
            return f"ubuntu-{self.osver}_{self.arch}"
        if self.os == "ios":
            return "ios"
        if self.os == "android":
            return "android"
        if self.os == "raspberry-pi-os":
            return f"raspberry-pi-os_{self.arch}"
        if self.os == "jetson":
            if self.extra is None:
                ubuntu_version = "ubuntu-20.04"
            else:
                ubuntu_version = self.extra
            if self.osver is None:
                return f"{ubuntu_version}_armv8_jetson"
            return f"{ubuntu_version}_armv8_jetson_{self.osver}"
        raise Exception("error")


def get_windows_osver():
    osver = platform.release()
    # Windows 以外の環境だと reportAttributeAccessIssue を報告されてしまうので ignore する
    with winreg.OpenKeyEx(  # type: ignore[reportAttributeAccessIssue]
        winreg.HKEY_LOCAL_MACHINE,  # type: ignore[reportAttributeAccessIssue]
        "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion",
    ) as key:
        return osver + "." + winreg.QueryValueEx(key, "ReleaseId")[0]  # type: ignore[reportAttributeAccessIssue]


def get_macos_osver():
    platform.mac_ver()[0]


def get_build_platform() -> PlatformTarget:
    os = platform.system()
    if os == "Windows":
        os = "windows"
        osver = get_windows_osver()
    elif os == "Darwin":
        os = "macos"
        osver = get_macos_osver()
    elif os == "Linux":
        release = read_version_file("/etc/os-release")
        os = release["NAME"]
        if os == "Ubuntu":
            os = "ubuntu"
            osver = release["VERSION_ID"]
        else:
            raise Exception(f"OS {os} not supported")
        pass
    else:
        raise Exception(f"OS {os} not supported")

    arch = platform.machine()
    if arch in ("AMD64", "x86_64"):
        arch = "x86_64"
    elif arch in ("aarch64", "arm64"):
        if os == "ubuntu":
            arch = "armv8"
        else:
            arch = "arm64"
    else:
        raise Exception(f"Arch {arch} not supported")

    return PlatformTarget(os, osver, arch)


def get_clang_version(clang):
    version_str = cmdcap([clang, "--version"])

    # version_str は以下のような文字列になっているので、ここからバージョンを取る
    #
    # clang version 16.0.0 (...)
    # Target: x86_64-unknown-linux-gnu
    # Thread model: posix
    # InstalledDir: /path/to/clang/bin
    #
    # Android 版だと以下のような文字列になっている
    #
    # Android (8490178, based on r450784d) clang version 14.0.6 (...)
    # Target: aarch64-unknown-linux-android29
    # Thread model: posix
    # InstalledDir: /path/to/android-ndk/toolchains/llvm/prebuilt/linux-x86_64/bin

    # clang version の次の文字列を取る
    xs = version_str.split("\n")[0].split(" ")
    for i in range(2, len(xs)):
        if xs[i - 2] == "clang" and xs[i - 1] == "version":
            return xs[i]

    raise Exception("Failed to get clang version")


def fix_clang_version(clang_dir, clang_version):
    # <clang_dir>/lib/clang/<clang_version>/include または
    # <clang_dir>/lib64/clang/<clang_version>/include が存在するか調べて、
    # 存在しない場合は clang_version を調節して、存在するバージョンに変換する
    #
    # <clang_dir>/lib/clang/16.0.0/include になっている場合と
    # <clang_dir>/lib/clang/16/include になっている場合があるため
    paths = [os.path.join(clang_dir, "lib", "clang"), os.path.join(clang_dir, "lib64", "clang")]
    exists = any(map(lambda x: os.path.exists(os.path.join(x, clang_version, "include")), paths))
    if exists:
        return clang_version

    fixed_clang_version = clang_version.split(".")[0]
    exists = any(
        map(lambda x: os.path.exists(os.path.join(x, fixed_clang_version, "include")), paths)
    )
    if exists:
        return fixed_clang_version

    raise Exception(
        f"Failed to fix clang version: clang_dir={clang_dir} clang_version={clang_version}"
    )


class Platform(object):
    def _check(self, flag):
        if not flag:
            raise Exception("Not supported")

    def _check_platform_target(self, p: PlatformTarget):
        if p.os == "raspberry-pi-os":
            self._check(p.arch in ("armv6", "armv7", "armv8"))
        elif p.os == "jetson":
            self._check(p.arch == "armv8")
        elif p.os in ("ios", "android"):
            self._check(p.arch is None)
        elif p.os == "ubuntu":
            self._check(p.arch in ("x86_64", "armv8"))
        else:
            self._check(p.arch in ("x86_64", "arm64", "hololens2"))

    def __init__(self, target_os, target_osver, target_arch, target_extra=None):
        build = get_build_platform()
        target = PlatformTarget(target_os, target_osver, target_arch, target_extra)

        self._check_platform_target(build)
        self._check_platform_target(target)

        if target.os == "windows":
            self._check(target.arch in ("x86_64", "arm64", "hololens2"))
            self._check(build.os == "windows")
            self._check(build.arch == "x86_64")
        if target.os == "macos":
            self._check(build.os == "macos")
            self._check(build.arch in ("x86_64", "arm64"))
        if target.os == "ios":
            self._check(build.os == "macos")
            self._check(build.arch in ("x86_64", "arm64"))
        if target.os == "android":
            self._check(build.os in ("ubuntu", "macos"))
            if build.os == "ubuntu":
                self._check(build.arch == "x86_64")
            elif build.os == "macos":
                self._check(build.arch in ("x86_64", "arm64"))
        if target.os == "ubuntu":
            self._check(build.os == "ubuntu")
            self._check(build.arch in ("x86_64", "armv8"))
            if build.arch == target.arch:
                self._check(build.osver == target.osver)
        if target.os == "raspberry-pi-os":
            self._check(build.os == "ubuntu")
            self._check(build.arch == "x86_64")
        if target.os == "jetson":
            self._check(build.os == "ubuntu")
            self._check(build.arch == "x86_64")

        self.build = build
        self.target = target


def get_webrtc_platform(platform: Platform) -> str:
    # WebRTC
    if platform.target.os == "windows":
        return f"windows_{platform.target.arch}"
    elif platform.target.os == "macos":
        return f"macos_{platform.target.arch}"
    elif platform.target.os == "ios":
        return "ios"
    elif platform.target.os == "android":
        return "android"
    elif platform.target.os == "ubuntu":
        return f"ubuntu-{platform.target.osver}_{platform.target.arch}"
    elif platform.target.os == "raspberry-pi-os":
        return f"raspberry-pi-os_{platform.target.arch}"
    elif platform.target.os == "jetson":
        if platform.target.extra is None:
            return "ubuntu-20.04_armv8"
        else:
            return f"{platform.target.extra}_armv8"
    else:
        raise Exception(f"Unknown platform {platform.target.os}")


# 内部で os.path.abspath() を利用しており、 os.path.abspath() はカレントディレクトリに依存するため、
# この関数を利用する場合は ArgumentParser.parse_args() 実行前にカレントディレクトリを変更してはならない
#
# また、 --sora-args の指定には `--sora-args='--test'` のように `=` を使う必要がある
# `--sora-args '--test'` のようにスペースを使うと、ハイフンから始まるオプションが正しく解釈されない
def add_sora_arguments(parser):
    parser.add_argument(
        "--local-sora-cpp-sdk-dir",
        type=os.path.abspath,
        default=None,
        help="Refer to local Sora C++ SDK. "
        "When this option is specified, Sora C++ SDK will also be built.",
    )
    parser.add_argument(
        "--local-sora-cpp-sdk-args",
        type=shlex.split,
        default=[],
        help="Options for building local Sora C++ SDK when `--local-sora-cpp-sdk-dir` is specified.",
    )


# add_sora_arguments と同様の注意点があるので注意すること
def add_webrtc_build_arguments(parser):
    parser.add_argument(
        "--local-webrtc-build-dir",
        type=os.path.abspath,
        default=None,
        help="Refer to local webrtc-build. "
        "When this option is specified, webrtc-build will also be built.",
    )
    parser.add_argument(
        "--local-webrtc-build-args",
        type=shlex.split,
        default=[],
        help="Options for building local webrtc-build when `--local-webrtc-build-dir` is specified.",
    )
