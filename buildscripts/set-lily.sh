#!/bin/sh
# set-lily.sh --- configure LilyPond sourcetree; 
#
# configure Lily in $HOME/usr/src/lilypond-x.x.x, 
# and without installing
#

PACKAGE_NAME=LilyPond
export PACKAGE_NAME
prefix=$HOME/usr
sources=$prefix/src

showln ()
{
	(set -x; ln $*)
}

testmkdir ()
{
	if [ ! -x $1 ]; then
		(set -x; mkdir -p $1)
	fi
}

testvar ()
{
	var=`eval echo '\$'$1`
	if [ "`echo "$var" | grep $2`" = "" ]; then
#	if ! expr "$var" : ".*\($2\).*" ; then
		eval $1=$3 
		export $1
		echo "        $1=$3"
		echo "        export $1"
	fi
}

. ./stepmake/bin/package-zet.sh

echo You should add the following to your profile script
echo
testvar LILYPONDPREFIX lily $LILYPOND_SOURCEDIR
testvar MFINPUTS lily .:$MFINPUTS:$LILYPOND_SOURCEDIR/mf
testvar TEXINPUTS lily .:$TEXINPUTS:$LILYPOND_SOURCEDIR/ps:$LILYPOND_SOURCEDIR/tex
testvar GS_LIB lily $HOME/usr/src/lilypond/ps
testvar GS_FONTPATH lily $HOME/usr/src/lilypond/mf/out
testvar GUILE_LOAD_PATH lily $HOME/usr/src/lilypond/init
testvar MAILADDRESS "@" $USER@`hostname`
echo

echo Setting up links
echo
showln -sf $LILYPOND_SOURCEDIR/lily/out/lilypond $prefix/bin/lilypond
showln -sf $LILYPOND_SOURCEDIR/mi2mu/out/mi2mu $prefix/bin/mi2mu
showln -sf $LILYPOND_SOURCEDIR/scripts/out/ly2dvi $prefix/bin/ly2dvi
showln -sf $LILYPOND_SOURCEDIR/scripts/out/mudela-book $prefix/bin/mudela-book
chmod 755 $LILYPOND_SOURCEDIR/buildscripts/ps-to-gifs.sh
showln -sf $LILYPOND_SOURCEDIR/buildscripts/ps-to-gifs.sh $prefix/bin/ps-to-gifs

testmkdir $prefix/share
rm -rf $prefix/share/lilypond
showln -sf $sources/lilypond $prefix/share/lilypond

if [ -f ../.gdbinit ];
then
    showln -f ../.gdbinit .
fi

if [ -f ../.dstreamrc ]
then
    showln -f ../.dstreamrc .
fi
echo

echo Starting configuration
echo
(set -x; TEX_TFMDIR=$TEX_TFMDIR ./configure --prefix=$prefix --enable-debugging --enable-printing --enable-checking --disable-optimise --enable-guile)

