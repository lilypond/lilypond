#!/bin/sh
# check-lily-paper.sh

# out=check-lily.tex
# ugh -o is broken
out=lelie.tex
ok=$LILYPOND_SOURCEDIR/test/ok.tex
cat <<EOF | lilypond -T # -o $out
\score{
	\melodic{
		c d e f g a b c'
	}
	\paper{}
}
EOF

# ugh
diff $out $ok
exit $?

