\header {
	title = "The river is flowing";
	composer = "Traditonal (?)";
}

melody = \notes \relative c' {
	\partial 8;
	g8 |
	c4 c8 d [es () d] c4 | f4 f8 g [es() d] c g |
	c4 c8 d [es () d] c4 | d4 es8 d c4.
	\bar "|.";\clef bass;
}

text = \lyrics {
	The ri -- ver is flo -- wing, flo -- wing and gro -- wing, the
	ri -- ver is flo -- wing down to the sea.
}

accompaniment =\chords  \sequential {
	r8
	c2-min f-min 7 d-min es4 c8-min r8
	c2-min f-min 7 g c-min
}

\score {
	\simultaneous {
	  \context ChordNames \accompaniment
	  \addlyrics
	     \context Staff {
	    	\property Voice.beamAuto = "0"
		\property Staff.automaticMelismas = "1"
	  	\melody }
	     \context LyricVoice \text
	}
	\paper { }
	\midi  { }
}
