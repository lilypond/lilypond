#!/bin/sh
# zet-lily.sh --- configure LilyPond sourcetree
# nice in first character unique name
#
PACKAGE_NAME=LilyPond
export PACKAGE_NAME
prefix=$HOME/usr
sources=$prefix/src
#
#
. ./stepmake/bin/package-zet.sh
#
# The $sources dir looks like this:
#
# <SEE PATCHES.txt>
#
if [ "x$LILYINCLUDE" = "x" ]; then
	# we can try...
	echo you should add the following to your profile script
	if [ "x$MAILADDRESS" = "x" ]; then
		MAILADDRESS=$USER@`hostname`
		export MAILADDRESS
		echo "        MAILADDRESS=\$USER@`hostname`"
		echo "        export MAILADDRESS"
	fi
	LILYINCLUDE=$LILYPOND_SOURCEDIR/init:$LILYPOND_SOURCEDIR/input:$LILYPOND_SOURCEDIR/mf/out
	MFINPUTS=$MFINPUTS:$LILYPOND_SOURCEDIR/mf
	TEXINPUTS=$TEXINPUTS:$LILYPOND_SOURCEDIR/mf/out
	export LILYINCLUDE MFINPUTS TEXINPUTS
	cat <<EOF
	LILYINCLUDE=$LILYPOND_SOURCEDIR/init:$LILYPOND_SOURCEDIR/mf/out
	MFINPUTS=\$MFINPUTS:\$LILYPOND_SOURCEDIR/mf
	TEXINPUTS=\$TEXINPUTS:\$LILYPOND_SOURCEDIR/mf/out
	export LILYINCLUDE MFINPUTS TEXINPUTS
EOF

fi

ln -sf $LILYPOND_SOURCEDIR/lily/out/lilypond $prefix/bin/lilypond
ln -sf $LILYPOND_SOURCEDIR/mi2mu/out/mi2mu $prefix/bin/mi2mu
ln -sf $LILYPOND_SOURCEDIR/scripts/ly2dvi.sh $prefix/bin/ly2dvi
chmod 755 $LILYPOND_SOURCEDIR/buildscripts/ps-to-gifs.sh
ln -sf $LILYPOND_SOURCEDIR/buildscripts/ps-to-gifs.sh $prefix/bin/ps-to-gifs

if [ -f ../.gdbinit ];
then
    ln ../.gdbinit .
fi

if [ -f ../.dstreamrc ]
then
    ln ../.dstreamrc .
fi

./configure --prefix=$prefix --enable-debugging --enable-printing --enable-checking --disable-optimise --enable-guile

