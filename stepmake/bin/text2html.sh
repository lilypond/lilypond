#!/bin/sh
# text2html.sh

if [ $# -ne 2 ]; then
	echo "Usage: text2html TEXT-FILE HTML-FILENAME"
	exit 2
fi

infile=$1
outfile=$2
rm -f $outfile
echo "<body>" > $outfile
echo "<xmp>" >> $outfile
cat $infile >> $outfile
echo "</xmp>" >> $outfile
echo "</body>" >> $outfile

