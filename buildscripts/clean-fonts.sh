#!/bin/sh

case  $# in
0) 
    WHAT="" ;;
1)
    WHAT=$1;;
esac

# should use kpsepath 

TEXDIRS=.
if [ -d /var/lib/texmf ]; then
    TEXDIRS="$TEXDIRS /var/lib/texmf"
fi
if [ -d /var/spool/texmf ]; then
    TEXDIRS="$TEXDIRS /var/tmp/texmf"
fi
if [ -d /var/tmp/texfonts ]; then
    TEXDIRS="$TEXDIRS /var/spool/texfonts"
fi
if [ -d /var/texfonts ]; then
    TEXDIRS="$TEXDIRS /var/texfonts"
fi
if [ -z "$TEXDIR" ]; then
    TEXDIRS=". /var"
else
    TEXDIRS=". $TEXDIRS"
fi

# remove possibly stale .pk/.tfm files 
FILES=`find $TEXDIRS -name "feta*$WHAT*tfm" -or -name "feta*$WHAT*pk"`

echo removing $FILES
rm  -f $FILES /tmp/cleaning-font-dummy
