#!/bin/sh
# ps-to-gifs, convert PS to multiple gifs
  
usage()
{
    cat <<EOF
Usage: ps-to-gifs.sh [OPTION]... [FILE]
Options:
  -h, --help         this help
  -c, --crop         crop output
  -o, --output=NAME  set output base
  -t, --transparent  change white to transparent
EOF
}

if [ $# -lt 1 ]; then
    usage;
    exit 2;
fi
CROP=cat

while [ $# -gt 0 ]; do
opt=$1
shift
    case $opt in
    -t|--t*)
	color='-transparent white'
	;;
    -h|--h*)
	usage;
	exit 0
	;;
    -c|--c*)
	CROP=" pnmcrop "
	;;
    -o) OUTFILE=$2; shift
        ;;
    --o*=*) OUTFILE=`echo $opt | sed -e s/"^.*="//`
        ;;
    -*)
        echo "ps-to-gifs: unknown option: \`$opt'"
	exit 1
	;;
    *)
	FILE=$opt
	;;
    esac
done

if [ "x$TRANSPARENT_IS_BROKEN" != "x" ]; then
	color=
fi

if [ "x$OUTFILE" = "x" ]; then
	BASE=`dirname $FILE`/`basename $FILE .ps`
else
	BASE=`dirname $OUTFILE`/`basename $OUTFILE .gif`
fi

# urg, pipe breaks
rm -f $BASE{.ppm,.gif} $BASE-page*{.ppm,.gif}

# generate the pixmap at twice the size, then rescale (for antialiasing)
cat $FILE | gs -sDEVICE=ppmraw -sOutputFile="$BASE-page%d.ppm" -r200 -dNOPAUSE - -c quit $FILE
# quant is soo slow
# cat $PPMFILE | ppmquant 2 | pnmscale 0.3333 | pnmcrop | ppmtogif $color > $OUTFILE
PPMS=`ls $BASE*ppm`
for i in $PPMS; do
    o=`dirname $i`/`basename $i .ppm`.gif
    cat $i | pnmscale 0.5 | $CROP | ppmtogif $color > $o
    rm $i
done

if [ "x$OUTFILE" != "x" ]; then
	mv $BASE-page1.gif $BASE.gif
fi

