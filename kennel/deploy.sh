# ./deploy.sh <PREFIX> <PID_DIR> 
# ./deploy.sh /usr/local /var/run
PREFIX=$1
PID_DIR=$2
KENNEL=$PREFIX/wandbox/kennel

mkdir -p $KENNEL
cp -r static/ $KENNEL/static/
mkdir -p $KENNEL/config
cp config/settings.yml $KENNEL/config
cp config/sqlite.yml $KENNEL/config
mkdir -p $KENNEL/bin
cp cabal-dev/bin/kennel $KENNEL/bin

sed -e s#@PREFIX@#$PREFIX# kennel.rc.in > tmp
sed -e s#@PID_DIR@#$PID_DIR# tmp > kennel.rc
rm tmp
chmod +x kennel.rc
