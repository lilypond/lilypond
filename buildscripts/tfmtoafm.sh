#!@SHELL@
# tfmtoafm.sh --urg

if [ $# -ne 1 ]; then
	echo "Usage: tfmtoafm FONT"
	echo
	echo "example: tfmtoafm cmr10"
	exit 2
fi

font=$1
afm=$1.afm
fontfile=`locate $1.tfm`
t1=/tmp/tfmtoafm1-$$
t2=/tmp/tfmtoafm2-$$
rm -f $t1 $t2 $font $afm

for i in `seq 1 127`; do printf "%d Character-%d\n" $i $i >> $t1; done
tfmtodit $fontfile $t1 $font
rm $t1
tail -127 $font | cut -f 1-2 > $t1
cat $t1 | cut -d, -f 1-2 > $t2
rm $t1
nl -ba $t2 | sed "s/,/ /g" | sed "s/^ *//" | sed "s/$/ /" | sed "s/^[0-9]*/C &;/" | sed "s/\([0-9]\)\([0-9][0-9][0-9][0-9][0-9]\) /\1.\2 /"g | sed "s/-[0-9]*/&; B 0.00 0.00 /" > $t1
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

