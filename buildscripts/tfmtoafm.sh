#!@SHELL@
# tfmtoafm.sh --urg

if [ $# -ne 1 ]; then
	echo "Usage: tfmtoafm FONT"
	echo
	echo "example: tfmtoafm cmr10"
	exit 2
fi

font=$1
size=`echo $font | sed "s/[^0-9]*//"`
afm=$1.afm
fontfile=`kpsewhich $1.tfm`
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
nl -ba $t2 | awk -F '[ \t,]+' '{print "C "$2";\t"$3";\tB 0.00 0.00 "$4/'"$scaling"'" "$5/'"$scaling"'";"}' > $t1

cat > $afm <<EOF
FontName cmr
StartFontMetrics
StartCharMetrics
EOF
# urg 0 is difficult but we need it
head -1 $t1 | sed "s/1;/0;/g" >> $afm
cat $t1 >> $afm
cat >> $afm <<EOF
EndCharMetrics
EndFontMetrics
EOF
rm $t1 $t2

