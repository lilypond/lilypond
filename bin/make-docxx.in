#!/bin/sh
if [ x$LILYTOP = x ];
then
    LILYTOP = .
fi
p1=`find $LILYTOP -name '*.hh' |egrep -v out/` 
p2=`find $LILYTOP -name '*.cc' |egrep -v out/`
doc++ -f -d docxx/ -S -k -p $BANNEROPT $p1 $p2
