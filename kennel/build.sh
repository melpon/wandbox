
DIR=$(cd $(dirname $0);pwd)

mkdir -p cabal-dev/alex
mkdir -p cabal-dev/happy
cabal-dev install -s cabal-dev/alex alex || exit $?
cabal-dev install -s cabal-dev/happy happy || exit $?
PATH=$DIR/caeal-dev/alex/bin:$PATH
PATH=$DIR/cabal-dev/happy/bin:$PATH

cabal-dev install yesod-platform-1.0.0 || exit $?
cabal-dev install || exit $?

cp config/settings.yml.template config/settings.yml
cp config/sqlite.yml.template config/sqlite.yml
