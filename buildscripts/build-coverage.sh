#!/bin/sh
./configure --enable-config=cov --disable-optimising
make conf=cov -j2 clean
perl -i~ -pe 's/-pipe /-fprofile-arcs -ftest-coverage/g' config-cov.make
make conf=cov -j2
make conf=cov test-clean LILYPOND_JOBS=          
make conf=cov test LILYPOND_JOBS=          

cd out-cov
ln ../lily/* .
ln ../lily/out-conv/*cc .
mkdir include
ln ../lily/include/* include/
for a in *[cyl]
do
   gcov -o ../lily/out-cov/  -p $a > $a.gcov-summary
done 
