#!/bin/bash
## duh, rename executables,
## for people that use a dumb shell instead of bash

if [ $# -le 0 ]; then
	echo "Usage: fix-suffixes [FILE]..."
	exit 2
fi

echo `basename $0`

function fix_extension ()
{
        path=$1
        ext=$2
        expr="$3"
	dir=`dirname $path`
        file=`basename $path`
        base=`basename $file $ext`
        if [ $base$ext != $file ]; then
                type="`file $path`"
                if expr "$type" : "$expr"; then
                        mv -f $path $dir/$base$ext
                fi
        fi
}

for i in `/bin/ls -d1 $*`; do
	fix_extension $i .exe '.*Windows.*\(executable\).*'
#	fix_extension $i .py '.*\(python\).*'
done

