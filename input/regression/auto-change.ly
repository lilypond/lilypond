\header{
texidoc="
Auto change piano staff switches  voices  between up and down staffs
automatically; rests are switched along with the coming note.
";
}

\score {
	\notes \context PianoStaff <
	\context Staff = "up" {
		\autochange Staff \context Voice = VA < \relative c' { g4 a  b c d r4 a g } >
	}
	\context Staff = "down" {
		\clef bass; 
		s1*2
	}

	>
	
}
