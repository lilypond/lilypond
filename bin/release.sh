#!/bin/sh

# script to automate releases

grep -q  '^TOP' VERSION
    res=$?
if test ! -f VERSION  || test $res != 0; then
    echo not in topleveldir
    exit 1
fi    

function setversion() {
eval `sed -n 's/^\([A-Z_]*\) *= *\(.*\)$/\1=\2/p' VERSION`

MJ=$TOPLEVEL_MAJOR_VERSION
MI=$TOPLEVEL_MINOR_VERSION
PA=$TOPLEVEL_PATCH_LEVEL 
MP=$TOPLEVEL_MY_PATCH_LEVEL
NEWVER=$MJ.$MI.$PA$MP
if [ x$MP = x -o x$MP = xpre ]
then
    LASTVER=$MJ.$MI.`expr $PA - 1`

    if [ -f $releasedir//lilypond-$LASTVER""pre.tar.gz ] ; then
	LASTVER="$LASTVER""pre"
    fi
else
    LASTVER=$MJ.$MI.$PA
fi

echo
echo "Current  version ("`pwd`") is $NEWVER, Last version:  $LASTVER"
echo
}

heredir=`pwd`
releasedir=`pwd`/../releases
patchdir=`pwd`/../patches
MAKE=${MAKE:-"make"}
TAR=${TAR:-"tar"}


$MAKE dist; 
setversion
LILYVER=$NEWVER

tarball=lilypond-$LILYVER.tar.gz
patch=patch-$LILYVER.gz

mv  $tarball $releasedir/

cd ../test
sh $heredir/bin/make-patch.sh $LASTVER $NEWVER lilypond
gzip -f9 patch-$NEWVER
mv $patch $patchdir//

RPMS=`find ~/rpms/ -name lilypond-$NEWVER'*'rpm`
rm *.rpm {lilypond,patch}-*.gz

if [ ! -z "$RPMS" ]; then
    ln $RPMS . 
fi
    
ln $releasedir//$tarball .
ln $patchdir//$patch .    


if [ ! -z "$RPMS" ]; then
    RPMS="lilypond-$LILYVER-1.i386.rpm lilypond-$LILYVER-1.src.rpm"
fi    
$TAR cf updeet $tarball $patch $RPMS
$TAR tfv updeet

