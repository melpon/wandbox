#!/bin/bash

PROC=$1

rm -rf crash/
if [ -e /var/crash/*$PROC.*.crash ]; then
  apport-unpack /var/crash/*$PROC.*.crash crash/
  gdb `cat crash/ExecutablePath` -c crash/CoreDump
fi
