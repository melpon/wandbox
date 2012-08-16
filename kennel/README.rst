環境構築
========

1. ghc, cabal をインストール

::

 #Arch Linux の場合
 $ sudo pacman -S ghc cabal-install

2. cabal を使って cabal-dev をインストール

::

 $ cabal install cabal-dev
 # ~/.cabal/bin へのパスを通しておくこと

3. build.sh を実行

::

 $ ./build.sh

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

 $ sudo pacman -S start-stop-daemon

2. sqlite.yml, settings.yml の設定を確認する

::

 $ vim config/sqlite.yml
 $ vim config/settings.yml

本番では Production の設定が使われるので、各ファイルやフォルダを正しい場所に書き換えること。

3. deploy.sh を実行

::

 $ sudo ./deploy.sh /usr/local /var/run

この引数なら /usr/local/wandbox/kennel にそれぞれのファイルがコピーされる。
また、実行した際には /var/run に PID ファイルが作られる。

4. 起動スクリプトを実行

::

 $ sudo ./kennel.rc start

必要に応じて /etc/rc.d などに移動させること。
