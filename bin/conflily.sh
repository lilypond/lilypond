#!/bin/sh
# conflily --- configure LilyPond sourcetree
#
# You should set LILYPOND_SOURCEDIR to the latest uppacked source dir,
# it will default to:
#
export LILYPOND_SOURCEDIR=$HOME/lelie/current
#
# and create links to here, if necessary
#
if [ ! -e $HOME/lelie ]; then
	(cd ..; ln -s `pwd` $HOME/lelie)
fi
#
#
current=`basename \`pwd\``
(cd ..; rm -f current 2>&1 > /dev/null)
(cd ..; ln -s $current current)
#
# The LilyPond root dir looks like this:
#
#    current -> ./lilypond-x.x.x    symlink to current source
#    lilypond-x.x.x/                the unpacked LilyPond source
#    patches/                       gzipped patches
#    releases/                      tar gzipped releases
#    test/                          latest output of 'release'
#
#
if [ "x$LILYINCLUDE" = "x" ]; then
	# we can try...
	echo you should add the following to your login script
	if [ "x$MAILADDRESS" = "x" ]; then
		export MAILADDRESS=$USER@`hostname`
		echo "        export MAILADDRESS=\$USER@`hostname`"
	fi
	export LILYINCLUDE=$LILYPOND_SOURCEDIR/init:$LILYPOND_SOURCEDIR/mf/out
	export PATH=$PATH$LILYPOND_SOURCEDIR/bin:
	export MFINPUTS=$MFINPUTS:$LILYPOND_SOURCEDIR/mf
	export TEXINPUTS=$TEXINPUTS:$LILYPOND_SOURCEDIR/mf/out
	cat <<EOF
	export LILYINCLUDE=\$LILYPOND_SOURCEDIR/init:\$LILYPOND_SOURCEDIR/mf/out
	export PATH=\$PATH:$LILYPOND_SOURCEDIR/bin
	export MFINPUTS=\$MFINPUTS:\$LILYPOND_SOURCEDIR/mf
	export TEXINPUTS=\$TEXINPUTS:\$LILYPOND_SOURCEDIR/mf/out
EOF
fi

ln -sf $LILYPOND_SOURCEDIR/lily/out/lilypond bin/out/lilypond
ln -sf $LILPPOND_SOURCEDIR/out/mi2mu bin/out/mi2mu

configure --prefix=/usr --enable-debugging --enable-printing --enable-checking --disable-optimise
