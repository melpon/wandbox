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
 $ cabal-dev install yesod-platform-1.0.0 --force-reinstalls
 $ cabal-dev install

テスト実行
========

::

 $ cd wandbox/kennel
 $ cabal-dev/bin/yesod --dev devel

config/settings.yml の Development の設定が使われるので、適宜良い感じにしておくこと。
