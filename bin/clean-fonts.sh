#!/bin/sh

case  $# in
0) 
    WHAT="" ;;
1)
    WHAT=$1;;
esac
    
# remove possibly stale .pk/.tfm files 
echo> /tmp/cleaning-font-dummy
FILES=`find /var/lib/texmf/ -name "feta*$WHAT*"`

echo removing $FILES
rm  $FILES /tmp/cleaning-font-dummy
