#!/bin/sh
# stepdirs.sh

help () {
	echo "Usage: stepmake/bin/stepdirs.sh"
}

if [ $# -ne 0 ]; then
	help
	exit 2
fi

if [ ! -r stepmake ]; then
	help
	exit 2
fi

if [ -r VERSION ]; then
	. ./VERSION
	if [ "x$PACKAGE_NAME" = "xStepMake" ]; then
		help
		exit 2
	fi
fi

dirs="RedHat doos patches releases test"

echo -n "Checking StepMake layout..."
for i in $dirs; do
	if [ ! -r ../$i ]; then
		missing="$missing ../$i"
	fi
done

if [ "x$missing" = "x" ]; then
	echo ok
	exit 0
fi

echo

echo "Creating missing directories:"
for i in $missing; do
	(set -x; mkdir $i)
done

echo done
