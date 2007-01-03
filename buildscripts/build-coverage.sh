#!/bin/sh

if test "$1" == "--fresh"; then
  fresh=yes
fi

if test ! -f config-cov.make; then
  fresh=yes
fi

if test "$fresh" = "yes";
then
  ./configure --enable-config=cov --disable-optimising \
   &&   make conf=cov -j2 clean \
   &&   perl -i~ -pe 's/-pipe /-fprofile-arcs -ftest-coverage -pipe /g' config-cov.make \
   &&   perl -i~ -pe 's/ -ldl / -lgcov -ldl /g' config-cov.make
else
  find -name '*.gcda' -exec rm  '{}' ';'
fi


make conf=cov -j2 &&  \
  make conf=cov test-real-clean LILYPOND_JOBS= && \
  make conf=cov test LILYPOND_JOBS= >& out-cov/test-run.log

if test "$?" != "0"; then
  tail -100 out-cov/test-run.log
  exit 1
fi

rm -rf out-cov
mkdir out-cov

cd out-cov
ln ../lily/* .
ln ../lily/out-cov/*[ch] .
mkdir include
ln ../lily/include/* include/
for a in *[cyl]
do
   gcov -o ../lily/out-cov/  -p $a > $a.gcov-summary
done 

cat <<EOF

now run 

         python buildscripts/coverage.py

EOF
