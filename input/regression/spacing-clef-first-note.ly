\version "1.9.0"
\header {

    texidoc = "Clef changes at the start of a line get much more space
than clef changes halfway the line."

}

\score  {\notes  {
    < \context Staff = SA {
	c'2
	\clef bass e16 f a
	\clef treble b
	}
      \context Staff = SB  {
	  c'4 c'4 c'4 
      }>
    }
    \paper { raggedright = ##t
\translator { \StaffContext
	      TimeSignature = \turnOff
	 }

     }}

