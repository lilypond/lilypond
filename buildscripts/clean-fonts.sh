#!/bin/sh

case  $# in
0) 
    WHAT="" ;;
1)
    WHAT=$1;;
esac

# should use kpsepath 

if [ -d /var/lib/texmf ]; then
    TEXDIR=/var/lib/texmf
elif [ -d /var/texfonts ]; then
    TEXDIR=/var/texfonts
else
    TEXDIR=/var/
fi

# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font-dummy
FILES=`find .  $TEXDIR -name "feta*$WHAT*tfm" -or -name "feta*$WHAT*pk"`

echo removing $FILES
rm  -f $FILES /tmp/cleaning-font-dummy
