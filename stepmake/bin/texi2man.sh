#!/bin/sh
# texi2man.sh

if [ $# -ne 2 ]; then
	echo "Usage: texi2man TEXINFO-FILE MAN-FILENAME"
	exit 2
fi

date=`date +%d/%b/%y`
#urg
package_name=LilyPond
#urg urg
version=0.1.71

infile=$1
outfile=$2
name=`basename $1 .texinfo`
name=`basename $name .texi`
name=`echo $name | tr '[A-Z]' '[a-z]'`
NAME=`echo $name | tr '[a-z]' '[A-Z]'`

echo .TH\ $NAME\ 1\ \"$date\" "$package_name Documentation" >$outfile

NEWLINE="
"
cat $infile | 
#urg, wish we could fix texi2roff :-(
sed 's!.*\\input .*texinfo.*!!' |
sed 's!^@chapter.*!@section NAME!' |
# sed 's!^@node *[Ii]nvoking.*!@section SYNOPSIS!' |
sed 's!^@node.*!!' |
# perl -pe "{s/\@node ([^,]*).*/\@section /;\$i=\$1; \$i=~tr [a-z] [A-Z]; chop; \$_.=\$i . \"\n\";}" |
perl -pe "{s/\@section (.*)/\@section /;\$i=\$1; \$i=~tr [a-z] [A-Z]; chop; \$_.=\$i . \"\n\";}" |
sed 's!^@subsection .*!!' |
sed 's!^@c *@texi2man@!@!' |
perl -pe "s/\@url{([^}]*)}/\$1/" |
perl -pe "s/\@email{([^},]*)}/<\$1>/" |
perl -pe "s/\@email{([^},]*),([^}]*)}/\$2, <\$1>/" |
sed "s!DESCRIPTION!& NEWLINE\
This manual page was automatically generated from $infile by $0.  For more details, please refer to the info pages: NEWLINE\
    info $name NEWLINE\
!" |
sed "s!NEWLINE!\\$NEWLINE!g" |
# debugging:
tee $infile.texi2roff |
texi2roff -ms |
sed 's!^\.ds __ !.SH !' |
# /\&\n(H1.\n(H2      \*(__
sed 's!.*(H1.*!!' |
cat >> $outfile

