#!/bin/sh
# doos-dist.sh --- make a windoze distribution

latest=lilypond-`show-latest -p`
{ cd $LILYPOND_SOURCEDIR; cd ..; 
export LILYPOND_ROOTDIR=`pwd` }

if [ ! -e $LILYPOND_ROOTDIR/doos/zip ]; then
	doo mkdir -p $LILYPOND_ROOTDIR/doos/zip
fi

set -x

cd $LILYPOND_ROOTDIR/doos || exit 1

rm -rf lilypond-*

tar xzf $LILYPOND_ROOTDIR/releases/$latest.tar.gz || exit 1
cd $latest

export PATH=/usr/doos/bin:$PATH
configure --host=i386-pc-linux --target=i386-pc-cygwin32 --prefix=/usr --enable-debugging --enable-printing --enable-checking

make
make doosdist

ln out/$latest.exe.zip $LILYPOND_ROOTDIR/doos/zip

