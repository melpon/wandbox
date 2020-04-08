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

cp session.key $BUILD_DIR/kennel2/.session.key
mkdir -p _tmp

$BUILD_DIR/cattleshed/cattleshed -c $BUILD_DIR/cattleshed/cattleshed.conf -c compilers.default &
CATTLESHED_PID=$!

sleep 1

$BUILD_DIR/kennel2/kennel -c $BUILD_DIR/kennel2/kennel.json &
KENNEL_PID=$!

trap "kill $CATTLESHED_PID $KENNEL_PID" EXIT

sleep 2

RESULT="`curl -H "Content-type: application/json" -d @test.json  http://localhost:3500/api/compile.json | jq .program_output`"

if [ "$RESULT" != '"foo\n"' ]; then
  exit 1
fi
