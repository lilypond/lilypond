#!/bin/sh
# ps-to-gifs, convert PS to multiple gifs or other bitmaps

usage()
{
    cat <<EOF
Convert PS to multiple gifs or other bitmaps
Usage: ps-to-gifs.sh [OPTION]... [FILE]
Options:
  -h, --help         this help
  -c, --crop         crop output
  -o, --output=NAME  set output base
  -p, --png          convert to png
  -s, --size=SIZE    set papersize
  -t, --transparent  change white to transparent
EOF
}

if [ $# -lt 1 ]; then
    usage;
    exit 2;
fi
CROP=cat
GIF=gif
PNMTOGIF=ppmtogif

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
    -p|--p*)
	GIF=png
	PNMTOGIF=pnmtopng
	;;
    -s) SIZE="-sPAPERSIZE=$2"; shift
        ;;
    --s*=*)
        SIZE="-sPAPERSIZE=`echo $opt | sed -e s/"^.*="//`"
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
	BASE=`dirname $OUTFILE`/`basename $OUTFILE .$GIF`
fi

# urg, pipe breaks
rm -f $BASE{.ppm,.$GIF} $BASE-page*{.ppm,.$GIF}

# generate the pixmap at twice the size, then rescale (for antialiasing)
cat $FILE | gs -sDEVICE=ppmraw $SIZE -sOutputFile="$BASE-page%d.ppm" -r180 -dNOPAUSE - -c quit $FILE
# quant is soo slow
# cat $PPMFILE | ppmquant 2 | pnmscale 0.3333 | pnmcrop | $PNMTOGIF $color > $OUTFILE
PPMS=`ls $BASE*ppm`
for i in $PPMS; do
    o=`dirname $i`/`basename $i .ppm`.$GIF
    cat $i | pnmscale 0.5 | $CROP | $PNMTOGIF $color > $o
    rm $i
done

if [ "x$OUTFILE" != "x" ]; then
	mv $BASE-page1.$GIF $BASE.$GIF
fi

