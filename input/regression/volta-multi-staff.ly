#(ly:set-option 'old-relative)
\header {

    texidoc = "By setting @code{voltaOnThisStaff}, repeats can be put on more staffs in a score."

}
\version "1.9.0"


vmus = \notes { \repeat volta 2 c1 \alternative { d e } } 

\score  {

    \notes \relative c'' <
	\context StaffGroup = SGA <
	    \context Staff \vmus
	    \context Staff = SB \vmus
	>
	\context StaffGroup = SGB <
	    \context Staff =SC <
		\property Staff.voltaOnThisStaff = ##t
		\vmus >
	    \context Staff = SD \vmus
	>
    >

    \paper { raggedright = ##t }
}
