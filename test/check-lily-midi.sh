#!/bin/sh
# check-lily-midi.sh

# out=check-lily.midi
# ugh -o is broken
out=lelie.midi
ok=$LILYPOND_SOURCEDIR/test/ok.midi
cat <<EOF | lilypond -T -o $out
\score{
	\melodic{
		c d e f g a b c'
	}
	\midi{}
}
EOF

# ugh
cmp $out $ok
exit $?

