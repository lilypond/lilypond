#!/bin/bash
  
usage()
{
    echo 'ps-to-gifs.sh FILE.ps'
    exit 2;
}

case $# in
1)
    FILE=`basename $1 .ps`
    ;;
*)
    usage
    ;;
esac

# generate the pixmap at twice the size, then rescale (for antialiasing)
cat $1 | gs  -q -sDEVICE=ppmraw \
    -sOutputFile="|pnmscale 0.3333|ppmtogif > $FILE-page%d.gif" \
    -r200 -dNOPAUSE - -c quit

