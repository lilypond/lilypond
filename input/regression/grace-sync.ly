\header  {
  texidoc = "grace notes in different voices/staves are synchronized."
}

\score  {\notes < \context Staff  { c2
	 \grace  c8
  c2 c4 }
		\context Staff = SB { c2 \clef bass
 \grace { [dis8 ( d8] }

  ) c2 c4 }
		\context Staff = SC { c2 c2 c4 }
		>
		\paper { linewidth = -1. }
 } 
