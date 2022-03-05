#!/bin/bash

cd "`dirname $0`"

BUILD_DIR="_build/release"

while [ $# -ne 0 ]; do
  case "$1" in
    "--tsan" )
      BUILD_DIR="_build/tsan"
      ;;
    "--asan" )
      BUILD_DIR="_build/asan"
      ;;
  esac
  shift 1
done

set -ex

mkdir -p _tmp

$BUILD_DIR/cattleshed/cattleshed --log-level=trace -c $BUILD_DIR/cattleshed/cattleshed.conf -c ../cattleshed/compiler.default &
CATTLESHED_PID=$!

sleep 1

$BUILD_DIR/kennel/kennel --log-level=trace --sponsorsfile "" &
KENNEL_PID=$!

trap "kill $CATTLESHED_PID $KENNEL_PID" EXIT

sleep 2

URL="http://localhost:3500"
CURL="curl -H X-Real-IP:127.0.0.1"

# list.json のテスト
$CURL -f $URL/api/list.json > _tmp/actual_api_list.json
if ! diff -u assets/expected_api_list.json _tmp/actual_api_list.json; then
  echo "failed test /api/list.json" 1>&2
  exit 1
fi

# コンパイルのテスト
$CURL -f -H "Content-type: application/json" -d @assets/test.json  $URL/api/compile.json > _tmp/actual_api_compile.json
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.json" 1>&2
  exit 1
fi

$CURL -v -f -H "Content-type: application/json" -d @assets/test.json  $URL/api/compile.ndjson > _tmp/actual_api_compile.ndjson
if ! diff -u assets/expected_api_compile.ndjson _tmp/actual_api_compile.ndjson; then
  echo "failed test /api/compile.ndjson" 1>&2
  exit 1
fi

# permlink あたりのテスト

# 以下のような結果から jq する
#{"permlink":"g0hqobBtg6a3sEfO","program_message":"foo\n","program_output":"foo\n","status":"0","url":"https://wandbox.org/permlink/g0hqobBtg6a3sEfO"}
PERMLINK=`$CURL -f -H "Content-type: application/json" -d @assets/test2.json  $URL/api/compile.json | jq -r .permlink`

# 以下のような結果から jq する
# {"parameter":{"code":"echo foo","compiler":"bash","compiler-info":{"compiler-option-raw":false,"display-compile-command":"bash prog.sh","display-name":"bash","language":"Bash script","name":"bash","provider":0,"runtime-option-raw":true,"switches":[],"templates":["bash"],"version":"4.4.20(1)-release"},"compiler-option-raw":"","created_at":1586592986,"description":"","github_user":"","is_private":false,"options":"","runtime-option-raw":"","stdin":"","title":""},"result":{"program_message":"foo\n","program_output":"foo\n","status":"0"},"results":[{"data":"Start","type":"Control"},{"data":"foo\n","type":"StdOut"},{"data":"0","type":"ExitCode"},{"data":"Finish","type":"Control"}]}
OUTPUT=`$CURL -f $URL/api/permlink/$PERMLINK | jq .result.program_message`
if [ "$OUTPUT" != '"foo\n"' ]; then
  echo "failed test /permlink" 1>&2
  exit 1
fi

# https://github.com/melpon/wandbox/issues/299
$CURL -f -H "Content-type: application/json" -d @assets/test_issue299.json  $URL/api/compile.json > _tmp/actual_issue299.json
if ! diff -u assets/expected_issue299.json _tmp/actual_issue299.json; then
  echo "failed test /api/compile.json" 1>&2
  exit 1
fi

# 無限 fork でどうなるか確認する
OUTPUT_SIGNAL=`$CURL -f -H "Content-type: application/json" -d @assets/test_fork.json  $URL/api/compile.json | jq -r .signal`
if [ "$OUTPUT_SIGNAL" != "Killed" ]; then
  echo "failed test fork" 1>&2
  exit 1
fi
# 無限 fork の後も正常に動作するか確認する
$CURL -f -H "Content-type: application/json" -d @assets/test.json  $URL/api/compile.json > _tmp/actual_api_compile.json
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test fork" 1>&2
  exit 1
fi
# cattlegrid プロセスは１個もいないはず
if [ `ps -ef | grep cattlegrid | grep -v grep | wc -l` -ne 0 ]; then
  ps -ef | grep cattlegrid | grep -v grep
  echo "failed test fork" 1>&2
  exit 1
fi

# 無限 fork その２
OUTPUT_SIGNAL=`$CURL -f -H "Content-type: application/json" -d @assets/test_fork2.json  $URL/api/compile.json | jq -r .signal`
if [ "$OUTPUT_SIGNAL" != "File size limit exceeded" ]; then
  echo "failed test fork2" 1>&2
  exit 1
fi
$CURL -f -H "Content-type: application/json" -d @assets/test.json  $URL/api/compile.json > _tmp/actual_api_compile.json
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.json" 1>&2
  exit 1
fi
if [ `ps -ef | grep cattlegrid | grep -v grep | wc -l` -ne 0 ]; then
  ps -ef | grep cattlegrid | grep -v grep
  echo "failed test fork" 1>&2
  exit 1
fi

# 無限出力
set +e
OUTPUT_SIGNAL=`$CURL -f -H "Content-type: application/json" -d @assets/test_outputflood.json  $URL/api/compile.json | jq -r .signal`
if [ "$OUTPUT_SIGNAL" != "File size limit exceeded" ]; then
  echo "failed test fork2" 1>&2
  exit 1
fi

# 今は IP Limit 入れてないのでコメントアウト
# # 3秒で1000バイト以上送信するとブロックされるか確認
# # test_iplimit.json は 500 バイトちょっとのサイズになっている
# $CURL -f -H "Content-type: application/json" -d @assets/test_iplimit.json  $URL/api/compile.json
# # ここで 400 エラー
# ! $CURL -f -H "Content-type: application/json" -d @assets/test_iplimit.json  $URL/api/compile.json
# # ３秒以上待つ
# sleep 4
# $CURL -f -H "Content-type: application/json" -d @assets/test_iplimit.json  $URL/api/compile.json

# 大量のファイル書き込みで SIGKILL が発生するか確認
OUTPUT_SIGNAL=`$CURL -f -H "Content-type: application/json" -d @assets/test_storage.json  $URL/api/compile.json | jq -r .signal`
if [ "$OUTPUT_SIGNAL" != "Killed" ]; then
  echo "failed test storage" 1>&2
  exit 1
fi

echo "e2e test succeeded"

