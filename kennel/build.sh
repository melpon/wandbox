
cabal install cabal-dev
PATH=~/.cabal/bin:$PATH

mkdir -p cabal-dev/alex
mkdir -p cabal-dev/happy
cabal-dev install -s cabal-dev/alex alex
cabal-dev install -s cabal-dev/happy happy
PATH=cabal-dev/alex/bin:$PATH
PATH=cabal-dev/happy/bin:$PATH

cabal-dev install

cp config/settings.yml.template config/settings.yml
