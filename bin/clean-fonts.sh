#!/bin/sh

# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font
FILES=`find /var/lib/texmf/ -name 'font-en-tja*' -o -name 'feta*'`
# FILES=`find /var/lib/texmf/ -name 'font-en-tja*' -o name 'feta-*" -o -name 'vette-beam*' -o -name 'dyn10*'`
echo removing $FILES
rm  $FILES /tmp/cleaning-font
