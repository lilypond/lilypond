#!/bin/sh
# package-zip.sh --- make a windoze distribution

set -x

if [ $# -lt 2 ]; then
	echo "Usage: package-zip.sh PACKAGE_SOURCEDIR FILE..."
	exit 2
fi

topdir=$1
shift
. $topdir/VERSION

VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
if [ "x$MY_PATCH_LEVEL" != "x" ]; then
	VERSION=$VERSION.$MY_PATCH_LEVEL
fi

package=`echo $PACKAGE_NAME | tr '[A-Z]' '[a-z]'`
name=$package-$VERSION
ZIP="zip -r -9"
builddir="/tmp/$package-doos"

{ cd $topdir/..; PACKAGE_ROOTDIR=`pwd`; export PACKAGE_ROOTDIR; }

zip="$PACKAGE_ROOTDIR/doos/zip/$name.exe.zip"


if [ ! -e $PACKAGE_ROOTDIR/doos/zip ]; then
	doo mkdir -p $PACKAGE_ROOTDIR/doos/zip
fi

set -x

cd $PACKAGE_ROOTDIR/doos || exit 1

rm -rf $package-*

tar xzf $PACKAGE_ROOTDIR/releases/$name.tar.gz || exit 1
cd $name

export PATH=/usr/doos/bin:$PATH
./configure --host=i386-pc-linux --target=i386-pc-cygwin32 --prefix=/usr --enable-debugging --enable-printing --enable-checking

make
make -C Documentation info || true
rm -rf $builddir
make prefix="$builddir/usr" DOTEXE=.exe install
make prefix="$builddir/usr" installextradoc

rm -f $zip
(cd $builddir; $ZIP $zip $*)
# urg
true "Wrote: $zip"

# huh?
# ln out/$name.exe.zip $zip

