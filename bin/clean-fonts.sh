#!/bin/sh

# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font
FILES=`find /var/lib/texmf/ -name 'font-en-tja*'` # -or -name 'vette-beam*'`
echo removing $FILES
rm  $FILES /tmp/cleaning-font
