#!/bin/sh

# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font
FILES=`find /var/lib/texmf/ -name 'font-en-tja*'`
# FILES=`find /var/lib/texmf/ -name 'font-en-tja*' -o -name 'vette-beam*' -o -name 'dyn10*'`
echo removing $FILES
rm  $FILES /tmp/cleaning-font
