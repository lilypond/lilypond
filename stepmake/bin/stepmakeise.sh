#!/bin/sh
# stepmakeise.sh

help () {
	cat <<EOF
Usage: ../stepmake-x.x.x/bin/stepmakeise.sh
Include or update package's StepMake.
EOF
}

name=`basename $0`
reldir=../releases
stepbin=`dirname $0`

if [ $# -ne 0 ]; then
	help
	exit 2
fi

value ()
{
    expression=`echo $1 | sed 's/\./ \\\\\* 100 + /g'`
    # urg?  $1=`expr $expression`
    urg=/tmp/stepmakeise.$$
    echo expr $expression > $urg
    echo `. $urg`
    rm -f $urg
}

if [ -r stepmake ]; then
	. ./stepmake/VERSION
	if [ "x$PACKAGE_NAME" != "xStepMake" ]; then
		echo "$name: huh 1?"
		exit 1
	fi
	echo "Stepmake found"
	echo -n "Checking version..."
	VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_LEVEL
	# urg
	version=$VERSION
	if [ "$MY_PATCH_LEVEL" != "" ]; then
		VERSION=$VERSION.$MY_PATCH_LEVEL
	fi
	echo " $version"
else
	VERSION="0.0.0"
	version=$VERSION
fi

if [ true ]; then
	# urg
	echo -n "Checking latest..."
	if [ ! -r $reldir ]; then
		echo "$name: huh 2?"
		exit 1
	fi
	LATEST=`cd $reldir; ls -t1 stepmake-*.tar.gz | head -1 | sed 's!stepmake-!!' | sed 's!.tar.gz!!'`
	# urg
	latest=`echo $LATEST | sed 's/\.[a-zA-Z][a-zA-Z]*[0-9]*$//'`
	latest_val=`value $latest`
	echo " $latest"
	version_val=`value $version`
	if [ $latest_val -le $version_val ]; then
	    echo "relax, StepMake is up to date"
	    exit 0
	fi
	echo -n "Updating StepMake..."
	(set +x; rm -rf stepmake; tar xzf $reldir/stepmake-$LATEST.tar.gz; mv stepmake-$LATEST stepmake)
	echo "ok"
fi

if [ -r VERSION ]; then
	echo "$name: warning: VERSION found: not stepmakeising"
	echo "You should rerun configure"
	rm -f Makefile
	exit 0
fi

files="VERSION make aclocal.in configure.in config.hh.in config.make.in"
for i in $files; do
	if [ -r $i ]; then
		echo "$name: can't stepmakise: $i already present"
		exit 1
	fi
done

echo -n "Stepmakeising..."
for i in $files; do
	cp -prv stepmake/$i .
done

cat <<EOF
ok
Please edit the folowing files to your package's needs:

    VERSION
    configure.in 

and look at:

    make/Toplevel.make.in
    config.hh.in
    config.make.in
EOF

