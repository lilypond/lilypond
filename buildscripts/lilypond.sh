#!/bin/sh
# via scm wrapper

foo=lilypond.$$
trap "rm $foo" 1 2 15
touch lilypond.$$

$LILYPONDPREFIX/lily/out/lilypond -f scm $*
scm=`find . -maxdepth 1 -cnewer $foo -and -name '*.scm'`
for i in $scm; do
	dir=`dirname $i`
	base=`basename $i .scm`
	file=$dir/$base.tex
	rm -f $file
	guile -s $i > $file
done
rm -f $scm
rm $foo
