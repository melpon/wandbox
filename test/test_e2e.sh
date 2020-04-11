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

cp assets/session.key $BUILD_DIR/kennel2/.session.key
mkdir -p _tmp

$BUILD_DIR/cattleshed/cattleshed -c $BUILD_DIR/cattleshed/cattleshed.conf -c assets/compilers.default &
CATTLESHED_PID=$!

sleep 1

$BUILD_DIR/kennel2/kennel -c $BUILD_DIR/kennel2/kennel.json &
KENNEL_PID=$!

trap "kill $CATTLESHED_PID $KENNEL_PID" EXIT

sleep 2

# コンパイルのテスト
curl -f -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/api/compile.json > _tmp/actual_api_compile.json
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.json" 1>&2
  exit 1
fi

curl -f -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/compile > _tmp/actual_compile
if ! diff -u assets/expected_compile _tmp/actual_compile; then
  echo "failed test /compile" 1>&2
  exit 1
fi

curl -f -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/api/compile.ndjson > _tmp/actual_api_compile.ndjson
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.ndjson" 1>&2
  exit 1
fi

# permlink あたりのテスト

# 以下のような結果から jq する
#{"permlink":"g0hqobBtg6a3sEfO","program_message":"foo\n","program_output":"foo\n","status":"0","url":"https://wandbox.org/permlink/g0hqobBtg6a3sEfO"}
PERMLINK=`curl -f -H "Content-type: application/json" -d @assets/test2.json  http://localhost:3500/api/compile.json | jq -r .permlink`

# 以下のような結果から jq する
# {"parameter":{"code":"echo foo","compiler":"bash","compiler-info":{"compiler-option-raw":false,"display-compile-command":"bash prog.sh","display-name":"bash","language":"Bash script","name":"bash","provider":0,"runtime-option-raw":true,"switches":[],"templates":["bash"],"version":"4.4.20(1)-release"},"compiler-option-raw":"","created_at":1586592986,"description":"","github_user":"","is_private":false,"options":"","runtime-option-raw":"","stdin":"","title":""},"result":{"program_message":"foo\n","program_output":"foo\n","status":"0"},"results":[{"data":"Start","type":"Control"},{"data":"foo\n","type":"StdOut"},{"data":"0","type":"ExitCode"},{"data":"Finish","type":"Control"}]}
OUTPUT=`curl -f http://localhost:3500/api/permlink/$PERMLINK | jq .result.program_message`
if [ "$OUTPUT" != '"foo\n"' ]; then
  echo "failed test /permlink" 1>&2
  exit 1
fi

echo "e2e test succeeded"

