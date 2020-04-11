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

curl -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/api/compile.json > _tmp/actual_api_compile.json
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.json" 1>&2
  exit 1
fi

curl -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/compile > _tmp/actual_compile
if ! diff -u assets/expected_compile _tmp/actual_compile; then
  echo "failed test /compile" 1>&2
  exit 1
fi

curl -H "Content-type: application/json" -d @assets/test.json  http://localhost:3500/api/compile.ndjson > _tmp/actual_api_compile.ndjson
if ! diff -u assets/expected_api_compile.json _tmp/actual_api_compile.json; then
  echo "failed test /api/compile.ndjson" 1>&2
  exit 1
fi

echo "e2e test succeeded"
