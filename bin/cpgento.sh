#!/bin/sh
# cpgento
#

if test $# -ne 1
then
	echo "Usage: "
	echo "	cpgento LOCATION, e.g.:"; 
	echo
	echo "	cpgento /mnt/aix/usr/src/lily"
       	echo "	CP=rcp cpgento fred@pcnov095.win.tue.nl:music/lily"
	exit 1
fi

MAKE=${MAKE:-make}
CP=${CP:-cp}

genlily="out/parser.hh out/parser.cc out/lexer.cc"
echo generating $genlily ...
$MAKE -C lily $genlily

genmi2mu="out/midi-parser.hh out/midi-parser.cc out/midi-lexer.cc"
echo generating $genmi2mu ...
$MAKE -C mi2mu $genmi2mu

lilydir=`pwd | sed "s/.*\///"`
todir=$1/$lilydir
echo "copying $lilydir -> $todir"

cpto() {
	name=$1
	tostuff=$todir/$name/out
	genstuff="$2"
	if [ "$CP" = "cp" -a \! -d $tostuff ]
	then
		echo mkdir -p $tostuff
		mkdir -p $tostuff
	fi
	echo $CP $genstuff $tostuff
	(cd $name; $CP $genstuff $tostuff)
}

cpto lily "$genlily"
cpto mi2mu "$genmi2mu"

# if you cannot gen the above, you-ll probably want:
flexlexerh=/usr/include/FlexLexer.h
cpto lib $flexlexerh

