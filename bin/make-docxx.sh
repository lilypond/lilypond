#!/bin/sh
if [ x$LILYTOP = x ];
then
    LILYTOP = .
fi
p1=`find $LILYTOP -name '*.hh' |egrep -v out/` 
p2=`find $LILYTOP -name '*.cc' |egrep -v out/`
doc++ -d out/ -S -k -p $p1 $p2
