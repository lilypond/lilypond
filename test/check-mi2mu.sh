#!/bin/sh

out=check-mi2mu.ly
# out=check-lily.midi
# ugh -o is broken
out=test.midi.ly
ok=$LILYPOND_SOURCEDIR/test/ok.ly
mi2mu -T $LILYPOND_SOURCEDIR/test/test.midi # -o $out

# ugh
diff $out $ok
exit $?

