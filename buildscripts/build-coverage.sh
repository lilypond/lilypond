#!/bin/sh
./configure --enable-config=cov --disable-optimising
make conf=cov -j2 clean
perl -i~ -pe 's/-pipe /-fprofile-arcs -ftest-coverage -pipe /g' config-cov.make
perl -i~ -pe 's/ -ldl / -lgcov -ldl /g' config-cov.make
make conf=cov -j2
make conf=cov test-clean LILYPOND_JOBS=          
make conf=cov test LILYPOND_JOBS= >& out-cov/test-run.log

cd out-cov
ln ../lily/* .
ln ../lily/out-cov/*[ch] .
mkdir include
ln ../lily/include/* include/
for a in *[cyl]
do
   gcov -o ../lily/out-cov/  -p $a > $a.gcov-summary
done 
