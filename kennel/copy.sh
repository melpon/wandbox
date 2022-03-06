#!/bin/bash

cd `dirname $0`

# 本番サーバーから DB ファイルを拾ってきてローカルで確認する用のスクリプト
scp wandbox:/opt/wandbox-data/release/kennel-master/var/lib/kennel/kennel_production.sqlite kennel.sqlite
