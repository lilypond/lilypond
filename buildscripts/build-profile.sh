#!/bin/sh

if test "$1" == "--fresh"; then
  fresh=yes
fi

if test ! -f config-prof.make; then
  fresh=yes
fi

if test "$fresh" = "yes";
then
  ./configure --enable-config=prof --enable-optimising \
   &&   perl -i~ -pe 's/-pipe /-pg -pipe /g' config-prof.make \
   &&   perl -i~ -pe 's/ -ldl / -pg -ldl /g' config-prof.make
fi

make conf=prof -j2

if test "$?" != "0"; then
  exit 2
fi

depth=../..
resultdir=out/profile-results

rm -rf $resultdir
mkdir $resultdir
cd $resultdir


cat > long-score.ly << EOF
\version "2.10.0"
foo = \new Staff \new Voice \repeat unfold 50 \relative { c4 d8[ d16( e]~ e16[ e e) f] g8  }
\score { 
  \new ChoirStaff << 
    \foo \foo \foo \foo 
    \foo \foo \foo \foo 

  >>
  \midi {}
  \layout {}
}
EOF

rm gmon.sum

exe=$depth/out-prof/bin/lilypond

## todo: figure out representative sample.
files="wtk1-fugue2 wtk1-fugue2 wtk1-fugue2 wtk1-fugue2 mozart-hrn-3  mozart-hrn-3  long-score"



$exe -ddump-profile --formats=ps -I $depth/input/ -I  $depth/input/mutopia/J.S.Bach/ \
    -I $depth/input/mutopia/W.A.Mozart/ \
    $files


for a in *.profile; do
  echo $a
  cat $a
done

echo 'running gprof' 
gprof $exe > profile

exit 0


## gprof -s takes forever.
for a in seq 1 3; do
  for f in $files ; do
    $exe -ddump-profile --formats=ps -I $depth/input/ -I  $depth/input/mutopia/J.S.Bach/ \
       -I $depth/input/mutopia/W.A.Mozart/ \
       $f

    echo 'running gprof' 
    if test -f gmon.sum ; then
      gprof -s $exe gmon.out gmon.sum
    else
      mv gmon.out gmon.sum
    fi
  done
done

gprof $exe gmon.sum > profile
