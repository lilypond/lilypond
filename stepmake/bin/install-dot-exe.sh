#!/bin/sh
# install-dot-exe.sh  -- add .exe for cygnus gnu-windows
# hack for doos install

realinstall=install

args=''
while [ $# -ne 0 ] 
do
    x=`echo $1 | sed 's@//@/@g'`
    case $1 in
	-*) args="$args $x"
	    ;;

	 *) if [ -f $1.exe ]; then
		args="$args $x.exe"
	    else
		args="$args $x"
	    fi
	    ;;
    esac
    shift
done

$realinstall $args
