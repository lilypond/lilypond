\version "1.3.146"
\header{
texidoc="
Stem tremolos or rolls are tremolo signs that look like beam segments
crossing stems.  If the stem is in a beam, the tremolo must be parallel
to the beam.  If the stem is invisible (eg. on a whole note), the
tremolo must be centered on the note.
"
}


\score{
	\context Voice\notes \relative c''{
		\property Voice.TextScript \set #'direction = #1
		\property Voice.TextScript \set #'padding = #5
		a1:4^":4" a:8^":8" c:16^":16" a:32^":32" a^"x" a:^":"
		a4:4 c:8 a:16 c:32 a a: a2:
		\break
		\stemUp
		a4:32 a'4:64 
		\stemDown
		c,4:32 c,4:64
		\stemBoth
		c'8:16 c c c
		a': a a: a
		c,16:32 c: c c a16:32 a: a a
		c8:16 g: d: a:
		c8:32 f: b: e:
	}
}
