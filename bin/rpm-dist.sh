#!/bin/sh
# rpm-dist.sh --- make an rpm distribution

latest=lilypond-`show-latest -p`
{ cd $LILYPOND_SOURCEDIR; cd ..; 
export LILYPOND_ROOTDIR=`pwd` }

icon=lelie_icon.gif
rpmrc=$LILYPOND_SOURCEDIR/make/out/rpmrc
spec=$LILYPOND_SOURCEDIR/make/out/lilypond.spec

if [ ! -e $LILYPOND_ROOTDIR/releases/$icon ]; then
	make -C $LILYPOND_ROOTDIR/current/Documentation gifs
	ln $LILYPOND_ROOTDIR/current/Documentation/out/$icon $LILYPOND_ROOTDIR/releases
fi

# urg
user=`whoami`
if [ "x$user" != "xroot" ]; then
	echo only root wants to build RPMs
	exit 2
fi

if [ ! -e $LILYPOND_ROOTDIR/redhat/BUILD ]; then
	mkdir -p $LILYPOND_ROOTDIR/redhat/BUILD
	mkdir -p $LILYPOND_ROOTDIR/redhat/RPMS/i386
	mkdir -p $LILYPOND_ROOTDIR/redhat/SRPMS
fi

set -x
cd $LILYPOND_ROOTDIR/redhat || exit 1
rm -rf lilypond-*

rpm -ba --rcfile $rpmrc $spec

