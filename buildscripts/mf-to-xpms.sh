#!@SHELL@
# mf-to-xpms.sh

if [ $# -ne 1 ]; then
	echo Usage: mf-to-xpms feta20
	exit 2
fi

font=`basename $1 .mf`
mf=$font.mf
afm=out/$font.afm
PKTOPBM=pktopbm
MODE=ibmvga
RESOLUTION=110
named=yes

if [ ! -e out/$font.${RESOLUTION}pk ]; then
	mf "\\mode=${MODE}; \\input $mf"
	mv $font.${RESOLUTION}gf out
	rm -f $font.log $font.tfm
	gftopk out/$font.${RESOLUTION}gf out/$font.${RESOLUTION}pk
fi

# num=`grep "^C *[0-9]*;" $afm | tail -1 | sed "s!^C *\([^;]*\).*!\\1!"`
# num=66
# tex=out/$font.tex
# cat > $tex <<EOF
# \font\fetatwenty=feta20
# \fetatwenty
# \nopagenumbers
# \newcount\c\c64
# \char\c
# \loop\ifnum\c<$num\advance\c by1
# 	\vfill\eject
# 	\char\c
# \repeat
# \vfill\eject
# \end
# EOF

# for i in $NUMS; do
# 	$PKTOPBM out/$font.${RESOLUTION}pk -c $i out/$font-$i.pbm
# done

# numbered files
if [ "x$named" = "x" ]; then
	NUMS=`grep "^C *[0-9]*;" $afm | sed "s!^C *\([^;]*\).*!\\1!"`
	PBMS=`grep "^C *[0-9]*;" $afm | sed "s!^C *\([^;]*\).*!out/$font-\\1.pbm!"`
else
	NUMS=`grep "^C *[0-9]*;" $afm | sed "s!^C [^;]*; *N *\([^;]*\).*!\\1!"`
	PBMS=`grep "^C *[0-9]*;" $afm | sed "s!^C [^;]*; *N *\([^;]*\).*!out/$font-\\1.pbm!"`
fi

# $PKTOPBM out/$font.${RESOLUTION}pk -x 100 -y 100 $PBMS
$PKTOPBM out/$font.${RESOLUTION}pk $PBMS

for i in $NUMS; do
	ppmtoxpm out/$font-$i.pbm > out/$font-$i.xpm
done
rm -f $PBMS
