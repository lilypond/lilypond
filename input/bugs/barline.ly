\header {
	title = "The river is flowing";
	composer = "Traditonal (?)";
}

melody = \notes \relative c' {
	\partial 8;
	g8 |
	
	
	\bar "|.";\clef bass;
}



accompaniment =\chords  \sequential {
	r8
	c2-min f-min 7 
}

\score {
	\simultaneous {
	  \context ChordNames \accompaniment
	     \context Staff {
	  	\melody }
	}
	\paper { }
	\midi  { }
}
