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

rm -f ${srcdir}/config.cache
PYTHON=${PYTHON:-python} ${srcdir}/configure --prefix=${distdir} \
    --srcdir=${srcdir}

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

if ! make -C Documentation/man doc ; then
    echo "make -C documentation/man doc failed"
    exit 1
fi

#
# Post install clean up
#
CYGWIN_LIB=$PACKAGE_ROOTDIR/distfiles/winnt/cygwinb19.dll
if [ ! -e $CYGWIN_LIB ]; then
    echo "Unable to locate $CYGWIN_LIB"
    exit 1
fi

#
# copy cygwin lib into bin
#
cp $CYGWIN_LIB $distdir/bin

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
cp Documentation/man/out/*.txt $distdir/doc
mv $distdir/doc/ly2dvi32.txt $distdir/doc/ly2dvi.txt
cd $distdir/..
$ZIP_CMD $ZIP_FILE $name
echo "Wrote $ZIP_FILE"
rm -rf $name
exit 0


