#!/bin/sh
cwd=`pwd`

{ cd $LILYPOND_SOURCEDIR; cd ..; 
export LILYPOND_ROOTDIR=`pwd` }

if [ "$1" = "" ]; then
  VERSION=`show-current`
  TARBALL=$LILYPOND_ROOTDIR/releases/lilypond-$VERSION.tar.gz
  cwd=`pwd`
else
  TARBALL=$1
fi


tar-docxx $TARBALL
