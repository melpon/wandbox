#!/bin/bash

PROG=$0

function show_help() {
  echo "$PROG <remote> <kennel | cattleshed> <develop | master>"
}

if [ $# -lt 3 ]; then
  show_help
  exit 1
fi

REMOTE=$1
APP=$2
ENV=$3

cd `dirname $0`
PACKAGE_DIR="`pwd`/_package"

set -ex

if [ "$APP" != "kennel" -a "$APP" != "cattleshed" -a "$APP" != "canine" ]; then
  show_help
  exit 1
fi

if [ "$ENV" != "develop" -a "$ENV" != "master" ]; then
  show_help
  exit 1
fi

scp $PACKAGE_DIR/$APP-$ENV.tar.gz $REMOTE:/tmp/$APP-$ENV.tar.gz
ssh $REMOTE /bin/bash -c "
  set -ex
  mkdir -p /opt/wandbox-data/release
  pushd /opt/wandbox-data/release
    tar xf /tmp/$APP-$ENV.tar.gz
    rm /tmp/$APP-$ENV.tar.gz

    pushd $APP-$ENV
      if [ "$APP" = "cattleshed" ]; then
        setcap cap_sys_admin,cap_chown,cap_setuid,cap_setgid,cap_sys_chroot,cap_mknod,cap_net_admin=p bin/cattlegrid
      fi
      if [ "$APP" = "kennel" ]; then
        # データ置き場を作る
        mkdir -p var/lib/kennel
        # セッションキー
        if [ "$ENV" = "develop" ]; then
          echo "0123456789abcdef0123456789abcdef" > var/lib/kennel/.session.key
        fi
        chown -R ubuntu:ubuntu var/
      fi
    popd
  popd
  cp /opt/wandbox-data/release/$APP-$ENV/etc/$APP.service /etc/systemd/system/$APP-$ENV.service
  systemctl enable $APP-$ENV
  systemctl restart $APP-$ENV
"
