#!/bin/sh

#shift;
if test "x$1" = x;
then
    versionfile="VERSION"
else
    versionfile=$1;
fi

cat $versionfile| sed 's/#.*$//g'|sed 's/\([^ ]*\)[\t ]*=[ \t]*\([^ ]*\)$/#define \1 \"\2\"/g' 
echo
    
