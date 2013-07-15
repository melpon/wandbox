環境構築
========

1. ghc, haskell-platform をインストール

::

 #Ubuntu
 $ sudo apt-get install ghc haskell-platform

2. cabal を使って cabal-dev をインストール

::

 $ cabal install cabal-dev
 # ~/.cabal/bin へのパスを通しておくこと

3. インストール

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
