#!/bin/sh
# zet-step.sh --- configure StepMake sourcetree
# nice in first character unique name
#
export PACKAGE_NAME=StepMake
prefix=$HOME/usr
#
#
. ./stepmake/bin/package-zet.sh
#
# The package root dir looks like this:
#
# <SEE INSTALL.txt>
#
# ln -sf $STEPMAKEL_SOURCEDIR/src/stepmake/out/step $prefix/bin/step

./configure --prefix=$prefix

