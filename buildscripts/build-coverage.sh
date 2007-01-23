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

mkdir -p scripts/out-cov/
touch scripts/out-cov/midi2ly scripts/out-cov/midi2ly.1
make conf=cov -j2 &&  \
  make conf=cov test-clean OUT_TEST=testcov LILYPOND_JOBS= && \
  make conf=cov test OUT_TEST=testcov LILYPOND_JOBS='-dtrace-scheme-coverage '

if test "$?" != "0"; then
  tail -100 out-cov/test-run.log
  exit 1
fi

depth=../..
resultdir=out/coverage-results

rm -rf $resultdir
mkdir $resultdir
cd $resultdir

ln $depth/lily/* .
ln $depth/scm/*.scm .
mv $depth/input/regression/out-testcov/*.scm.cov .
ln $depth/ly/*.ly .
ln $depth/lily/out-cov/*[ch] .
mkdir include
ln $depth/lily/include/* include/
ln $depth/flower/include/* include/
for a in *[cl] *.yy
do
   gcov -o $depth/lily/out-cov/  -p $a > $a.gcov-summary
done 

python $depth/buildscripts/coverage.py --uncovered *.cc > uncovered.txt
python $depth/buildscripts/coverage.py --hotspots *.cc > hotspots.txt
python $depth/buildscripts/coverage.py --summary *.cc > summary.txt
python $depth/buildscripts/coverage.py --uncovered *.scm > uncovered-scheme.txt

head -20 summary.txt

cat <<EOF
results in

  out/coverage-results/summary.txt
  out/coverage-results/uncovered.txt
  out/coverage-results/uncovered-scheme.txt
  out/coverage-results/hotspots.txt

EOF
