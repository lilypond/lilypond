\version "1.5.68"

\header{
texidoc="
Even if a line is very tightly spaced, there will still be room
between prefatory matter and the following notes.  The space after the
prefatory is rigid.  In contrast, the space before the barline
must stretch like the space within the measure.

"
}
\score {
	\notes { \time 2/2 f''2 c'2 \time 2/2 }
	\paper { linewidth = 2.5 \cm
	indent = 0.0
	}
}
