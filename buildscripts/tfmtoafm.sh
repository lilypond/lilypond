#!@SHELL@
# tfmtoafm.sh --urg

tfm=$1
font=`basename $1 .tfm`

if [ $# -ne 1 -o "$tfm" = "$font" ]; then
	echo "Usage: tfmtoafm TFM-FILE"
	echo
	echo "example: tfmtoafm cmr10.tfm"
	exit 2
fi

size=`echo $font | sed "s/[^0-9]*//"`
afm=$font.afm
fontfile=`kpsewhich $font.tfm`
t1=/tmp/tfmtoafm1-$$
t2=/tmp/tfmtoafm2-$$
rm -f $t1 $t2 $font $afm

for i in `seq 1 127`; do printf "%d Character-%d\n" $i $i >> $t1; done
tfmtodit $fontfile $t1 $font
rm $t1
scaling=`awk -v OFMT='%.5f' '/designsize/ {print $2/'$size/1000} $font`
tail -127 $font | cut -f 1-2 > $t1
cat $t1 | cut -d, -f 1-2 > $t2
rm $t1
#nl -ba $t2 | awk -F '[ \t,]+' '{print "C "$2";\t"$3";\tB 0.00 0.00 "$4/'"$scaling"'" "$5/'"$scaling"'";"}' > $t1
nl -ba $t2 | awk -F '[ \t,]+' '{print "C "$2" ; WX 0 ; N "$3" ; B 0.00 0.00 "$4/'"$scaling"'" "$5/'"$scaling"'" ;"}' > $t1

count=`cat $t1 | wc -l`
count=$((count + 1))
cat > $afm <<EOF
FontName cmr
StartFontMetrics
StartCharMetrics $count
EOF
# urg 0 is difficult but we need it
head -1 $t1 | sed -e "s/C 1 ;/C 0 ;/" -e "s/-1 ;/-0 ;/" >> $afm
cat $t1 >> $afm
cat >> $afm <<EOF
EndCharMetrics
EndFontMetrics
EOF
rm $t1 $t2 $font

