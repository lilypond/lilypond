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
#urg.  LILYPOND_PREFIX, _SOURCEDIR, PATH, *INPUTS all broken
#sourcetree=`basename \`pwd\``
#(cd .. ; rm lilypond-devel ; showln -s $sourcetree lilypond-devel)


echo You should add the following to your profile script
echo
testvar LILYPONDPREFIX lily $LILYPOND_SOURCEDIR
testvar MFINPUTS lily .:$MFINPUTS:$LILYPOND_SOURCEDIR/mf
testvar TEXINPUTS lily .:$TEXINPUTS:$LILYPOND_SOURCEDIR/ps:$LILYPOND_SOURCEDIR/tex
testvar GS_FONTPATH lily $HOME/usr/src/lilypond/mf/out
testvar MAILADDRESS "@" $USER@`hostname`
echo

echo Setting up links
echo
showln -sf $LILYPOND_SOURCEDIR/lily/out/lilypond $prefix/bin/lilypond
showln -sf $LILYPOND_SOURCEDIR/midi2ly/out/midi2ly $prefix/bin/midi2ly
showln -sf $LILYPOND_SOURCEDIR/scripts/out/ly2dvi $prefix/bin/ly2dvi
showln -sf $LILYPOND_SOURCEDIR/scripts/out/mudela-book $prefix/bin/mudela-book
showln -sf $LILYPOND_SOURCEDIR/buildscripts/out/genheader $prefix/bin/genheader



testmkdir $prefix/share
rm -rf $prefix/share/lilypond
showln -sf $sources/lilypond $prefix/share/lilypond


BUILDDIR=`pwd`
LOCALES="de it nl"
for i in $LOCALES; do
	dir=$BUILDDIR/share/locale/$i/LC_MESSAGES
	if test ! -x $dir ; then
		mkdir -p $dir
	fi
	rm -f $dir/lilypond.mo 
	showln -sf $BUILDDIR/po/out/$i.mo $dir/lilypond.mo
done
rm -f afm; showln -sf $BUILDDIR/mf/out afm
rm -f tfm; showln -sf $BUILDDIR/mf/out tfm
if test "x$TEX_TFMDIR" = "x" ; then
	CMR10=`kpsewhich tfm cmr10.tfm`
	TEX_TFMDIR=`dirname $CMR10`
fi
rm -f cmtfm; showln -sf $TEX_TFMDIR $BUILDDIR/cmtfm


if [ -f ../.gdbinit.lilypond ];
then
    showln -f ../.gdbinit.lilypond .gdbinit
fi

if [ -f ../.dstreamrc ]
then
    showln -f ../.dstreamrc .
fi
echo

echo Starting configuration
echo
(set -x; TEX_TFMDIR=$TEX_TFMDIR ./configure --prefix=$prefix --enable-debugging --enable-printing --enable-checking --disable-optimise)

echo "making tags in background"
make TAGS >& log &
