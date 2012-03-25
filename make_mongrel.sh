#! /bin/bash
VERSION=1.0.2
BUILD_NAME=mongrel-$VERSION
set -x
mkdir $BUILD_NAME
mkdir $BUILD_NAME/doc
mkdir $BUILD_NAME/ebin
mkdir $BUILD_NAME/tbin
erlc -I include -o $BUILD_NAME/ebin src/*.erl
cp src/*.app $BUILD_NAME/ebin
erlc -I include -o $BUILD_NAME/tbin test/*.erl
erl -noshell -pa $BUILD_NAME/ebin -pa $BUILD_NAME/tbin -s test_all test $BUILD_NAME/tbin -s init stop
rm -r $BUILD_NAME/tbin
cp overview.edoc $BUILD_NAME/doc
./gen_doc.sh $BUILD_NAME/doc
rsync -p -r --exclude=".*" src $BUILD_NAME
rsync -p -r --exclude=".*" test $BUILD_NAME
rsync -p -r --exclude=".*" include $BUILD_NAME
rsync -p -r --exclude=".*" src_examples $BUILD_NAME
cp make_mongrel.sh $BUILD_NAME
cp gen_doc.sh $BUILD_NAME
rm $BUILD_NAME/doc/overview.edoc
rm $BUILD_NAME/doc/edoc-info
cp overview.edoc $BUILD_NAME
if [ -f $BUILD_NAME.zip ]; then
    rm $BUILD_NAME.zip
fi
zip -q $BUILD_NAME.zip -r $BUILD_NAME
rm -r $BUILD_NAME
exit 0

