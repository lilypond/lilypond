#!/bin/sh
# package-zip32.sh --- make a windoze formated distribution

set -x

if [ $# -lt 1 ]; then
	echo "Usage: $0 PACKAGE_SOURCEDIR"
	exit 2
fi

srcdir=$1
shift

. $srcdir/VERSION

VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
if [ "x$MY_PATCH_LEVEL" != "x" ]; then
    VERSION=$VERSION.$MY_PATCH_LEVEL
fi

package=`echo $PACKAGE_NAME | tr '[A-Z]' '[a-z]'`
name=$package-$VERSION
ZIP_CMD="zip -r -9"

here=`pwd`
cd $srcdir/.. 
PACKAGE_ROOTDIR=`pwd` 
export PACKAGE_ROOTDIR 
cd $here

RELEASE_DIR="$PACKAGE_ROOTDIR/bin.releases/winnt"
ZIP_FILE="$RELEASE_DIR/$name.bin.zip"


if [ ! -e $RELEASE_DIR ]; then
    mkdir -p $RELEASE_DIR
fi

distdir=/tmp/${name}

#
# Maybe we can get away without reconfiguring
#
# rm -f ${srcdir}/config.cache
# PYTHON=${PYTHON:-python} ${srcdir}/configure --prefix=${distdir} \
#     --srcdir=${srcdir}

if ! make ; then
    echo "make failed"
    exit 1
fi

# failure allowed
make -C Documentation info

if ! make install ; then
    echo "make install failed"
    exit 1
fi

if ! make -C Documentation/man WWW ; then
    echo "make -C documentation/man WWW failed"
    exit 1
fi

#
# Post install clean up
#
CYGWIN_LIB=$PACKAGE_ROOTDIR/distfiles/winnt/cygwin1.dll
if [ ! -e $CYGWIN_LIB ]; then
    echo "Unable to locate $CYGWIN_LIB"
    exit 1
fi

#
# copy cygwin lib into bin
#
cp $CYGWIN_LIB $distdir/bin

ASH_EXE=$PACKAGE_ROOTDIR/distfiles/winnt/ash.exe
if [ ! -e $ASH_EXE ]; then
    echo "Unable to locate $ASH_EXE"
    exit 1
fi

#
# copy ash into bin
#
cp $ASH_EXE $distdir/bin

GUILE_SCM=$PACKAGE_ROOTDIR/distfiles/winnt/ice-9
if [ ! -e $GUILE_SCM ]; then
    echo "Unable to locate $GUILE_SCM"
    exit 1
fi

#
# copy guile init files into share/lilypond
#
echo "copy $GUILE_SCM to $distdir/share/lilypond"
cp -r $GUILE_SCM $distdir/share/lilypond

#
# Rename python files to <filename>.py
#
mv $distdir/bin/ly2dvi $distdir/bin/ly2dvi.py
mv $distdir/bin/convert-mudela $distdir/bin/convert-mudela.py
mv $distdir/bin/mudela-book $distdir/bin/mudela-book.py

#
# copy man documentation to doc directory
#
mkdir $distdir/doc
cp Documentation/man/out/*.html $distdir/doc

#
# copy web documentation to web directory
#
mkdir $distdir/web
for i in index.html guile.patch angels.ly
do
  cp Documentation/ntweb/out/$i $distdir/web || exit 1
done

#
# Zip it up
#
cd $distdir/..
$ZIP_CMD $ZIP_FILE $name
echo "Wrote $ZIP_FILE"
rm -rf $name
exit 0


