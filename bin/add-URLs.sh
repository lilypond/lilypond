#!/bin/sh

# add URLs for broken perl 5.004

perl -i~ -pe 's! ([a-z]+://[^ \t]+)! <a href=$1>$1</a>!g' $*
