#!/bin/sh
# package-zet.sh --- help configure a StepMake package's sourcetree
# normally invoked by a script like:
# 
#   zet-pack.sh  ( e.g.: ". bin/zet-lily.sh")
#
PACKAGE=`echo $PACKAGE_NAME | tr '[a-z]' '[A-Z]'`
package=`echo $PACKAGE_NAME | tr '[A-Z]' '[a-z]'`
#
# You should set ${PACKAGE}_SOURCEDIR to the latest unpacked source dir,
# it will default to:
#
#  $sources/$package  (e.g.: LILYPOND_SOURCEDIR=/home/fred/usr/src/lilypond)
#


PACKAGE_SOURCEDIR=`eval echo '\$'${PACKAGE}_SOURCEDIR`
if [ "x$PACKAGE_SOURCEDIR" = "x" ]; then
    eval ${PACKAGE}_SOURCEDIR="$prefix/src/$package"
    export ${PACKAGE}_SOURCEDIR
    PACKAGE_SOURCEDIR=`eval echo '\$'${PACKAGE}_SOURCEDIR`
fi
#
# and create links to here, if necessary
#
sourcetree=`basename \`pwd\``
(cd ..; rm -f $package 2>&1 > /dev/null)
(cd ..; ln -s $sourcetree $package)
#
if [ ! -r $sources ]; then
	(cd ..; ln -s `pwd` $sources)
fi
#
mkdir -p $prefix/bin
result=`echo $PATH | grep "$HOME/usr/bin"`
if [ "x$result" = "x" ]; then
    PATH=$PATH:$HOME/usr/bin
fi
result=`echo $PATH | grep "$sources/$package/bin"`
if [ "x$result" = "x" ]; then
    PATH=$PATH:$sources/$package/bin
fi
