#!/bin/sh

case  $# in
0) 
    WHAT="" ;;
1)
    WHAT=$1;;
esac

if [ X$LILYPOND_SOURCEDIR = X ];
then
    LILYPOND_SOURCEDIR=..
fi

if [ -d /var/lib/texmf ]; then
    TEXDIR=/var/lib/texmf
elif [ -d /var/texfonts ]; then
    TEXDIR=/var/texfonts
else
    TEXDIR=/var/
fi

#if [ -f $LILYPOND_SOURCEDIR/config.status ]; then
#TEXDIR=`awk -F % '/TEXPREFIX/ {print $3}' $LILYPOND_SOURCEDIR/config.status`

    
# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font-dummy
FILES=`find $TEXDIR -name "feta*$WHAT*tfm" -or -name "feta*$WHAT*pk"`

echo removing $FILES
rm  $FILES /tmp/cleaning-font-dummy
