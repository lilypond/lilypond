#!/bin/sh
# install-layout.sh --- build an installation layout
#
prefix=$HOME/usr
dirs='bin src/patches src/releases lib/texmf/tex lib/texmf/mf/source/public'

for i in $dirs; do
	if [ ! -e $prefix/$i ]; then
		echo +mkdir -p $prefix/$i
		mkdir -p $prefix/$i
	fi
done
