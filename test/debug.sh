#!/bin/bash

PROC=$1

rm -rf _crash/
if [ -e /var/crash/*$PROC.*.crash ]; then
  apport-unpack /var/crash/*$PROC.*.crash _crash/
  gdb `cat _crash/ExecutablePath` -c _crash/CoreDump
fi
