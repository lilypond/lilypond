\version "1.5.68"
\header {

    texidoc = "clef changes at the start of a line get much more space
than clef changes halfway the line."

}

\score  {\notes {
    < \context Staff = SA {
	c2
	\clef bass e16 f a
	\clef treble b
	}
      \context Staff = SB  {
	  c4 c4 c4 
      }>
    }
    \paper { linewidth  = -1.
\translator { \StaffContext
	      TimeSignature = \turnOff
	 }

     }}
