#!/bin/sh

p1=`find ./ -name '*.hh' |egrep -v out/` 
p2=`find ./ -name '*.cc' |egrep -v out/`
doc++ -d out/ -S -k -p $p1 $p2
