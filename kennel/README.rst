環境構築
========

1. ghc, cabal をインストール

::

 #Arch Linux の場合
 $ pacman -S ghc cabal-install

2. cabal を使って cabal-dev をインストール

::

 $ cabal install cabal-dev
 # ~/.cabal/bin へのパスを通しておくこと

3. build.sh を実行

::

 ./build.sh

4. settings.yml, sqlite.yml を設定

::

 $ vim config/settings.yml
 $ vim config/sqlite.yml

テスト実行
========

::

 $ cabal-dev/bin/yesod --dev devel

./config/settings.yml の Development の設定が使われる。

デプロイ
========

1. start-stop-daemon をインストール

::

 $ pacman -S start-stop-daemon

2. ./deploy.sh を実行

::

 $ ./deploy.sh /usr/local /var/run

この引数なら /usr/local/wandbox/kennel にそれぞれのファイルがコピーされる。
また、実行した際には /var/run に PID ファイルが作られる。

3. 起動スクリプトを実行

::

 $ ./kennel.rc start

/usr/local/wandbox/kennel/config/settings.yml の Production の設定が使われる。

必要に応じて /etc/rc.d などに移動させること。
