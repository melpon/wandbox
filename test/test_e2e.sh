#!/bin/bash

cd "`dirname $0`"

set -ex

cp session.key _build/release/kennel2/.session.key
mkdir -p _tmp

_build/release/cattleshed/cattleshed -c _build/release/cattleshed/cattleshed.conf -c compilers.default &
CATTLESHED_PID=$!

_build/release/kennel2/kennel -c _build/release/kennel2/kennel.json &
KENNEL_PID=$!

trap "kill $CATTLESHED_PID $KENNEL_PID" EXIT

sleep 2

RESULT="`curl -H "Content-type: application/json" -d @test.json  http://localhost:3500/api/compile.json | jq .program_output`"

if [ "$RESULT" != '"foo\n"' ]; then
  exit 1
fi
