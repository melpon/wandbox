#!/bin/bash
source /etc/profile.d/boost.sh
cd src
exec ./server.exe < ../config
