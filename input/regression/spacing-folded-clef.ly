\header {
texidoc = "A clef can be folded below notes in a different staff, if
this doesn't disrupt the flow of the notes."
}

\score { \notes \relative c'' <
\context Staff = SA  { c4 [c16 c c  c] c4 c4 }
	\context Staff = SB { \clef bass c,2 \clef treble  c'2 }
	>

	\paper { linewidth = -1. }
	}
