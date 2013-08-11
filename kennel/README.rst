環境構築
========

1. ghc, haskell-platform をインストール

::

 #Ubuntu
 $ sudo apt-get install ghc haskell-platform

2. cabal を使って cabal-dev をインストール

::

 $ cabal update
 $ cabal install cabal-dev
 # ~/.cabal/bin へのパスを通しておくこと

3. submodule の追加

::

 $ cd wandbox
 $ git submodule init
 $ git submodule update
 $ cd kennel/static
 $ ln -s ../../submodules/ace-builds/src-min ace
 $ ln -s ../../submodules/polyfills polyfills

4. インストール

::

 $ cd wandbox/kennel
 $ cabal-dev install yesod-platform-1.2.3 --force-reinstalls
 $ cabal-dev install

テスト実行
========

::

 $ cd wandbox/kennel
 $ cabal-dev install yesod-bin
 $ cabal-dev/bin/yesod --dev devel

デフォルトでは config/settings.yml の Development の設定が使われる。

設定ファイルを指定するには、環境変数 CONFIG にパスを指定する。 ::

 $ CONFIG="config/settings_test.yml" cabal-dev/bin/yesod --dev devel

この場合は config/settings_test.yml の Development の設定が使われる。
