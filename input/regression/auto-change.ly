
\version "1.9.2"

\header {

texidoc=" Auto change piano staff switches voices between up
and down staves automatically rests are switched along with the coming
note. When central C is reached, we don't switch (by default).

"
}
    \paper { raggedright= ##t }

\score {
	\notes \context PianoStaff <
	\context Staff = "up" {
		\autochange Staff \context Voice = VA < \relative c' { g4 c e d c r4 a g } >
	}
	\context Staff = "down" {
		\clef bass 
		s1*2
	}

	>
	
}

