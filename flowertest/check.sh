#!/bin/sh

./test > result.test
# ugh
cmp result.test $LILYPOND_SOURCEDIR/flowertest/result
exit $?

