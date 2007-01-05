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


depth=../..
resultdir=out/profile-results

rm -rf $resultdir
mkdir $resultdir
cd $resultdir


echo 'foo = \new Staff \new Voice \repeat unfold 50 \relative { c4 d8[ d16( e]~ e16[ e e) f] g8  }
\new ChoirStaff << 
  \foo \foo \foo \foo 
  \foo \foo \foo \foo 

>>' > long-score.ly


../bin/lilypond -ddump-profile -I $depth/input/ -I  $depth/input/mutopia/J.S.Bach/ \
     -I $depth/input/mutopia/W.A.Mozart/ \
     wtk-fugue2 mozart-hrn-3  long-score

gprof ../bin/lilypond
