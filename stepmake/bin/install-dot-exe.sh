#!/bin/sh
# install-dot-exe.sh  -- add .exe for cygnus gnu-windows
# hack for doos install; cygnus should support rpm

realinstall=install

args=''
while [ $# -ne 0 ] 
do
    case $1 in
	-*) args="$args $1"
	    ;;

	 *) if [ -f $1.exe ]; then
		args="$args $1.exe"
	    else
		args="$args $1"
	    fi
	    ;;
    esac
    shift
done

$realinstall $args
