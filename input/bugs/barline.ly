
\header {
texidoc= "Staves that end half way a system should end at the bar line.";
}

melody = \notes \relative c' {
	\partial 8;
	g8 |
	\bar "|.";\clef bass;
}



accompaniment =\chords  \sequential {
	r8
	r2 r2
}

\score {
	\simultaneous {
	  \context ChordNames \accompaniment
	     \context Staff {
	  	\melody }
	}
	\paper { }
}
