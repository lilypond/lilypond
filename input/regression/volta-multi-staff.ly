\header {

    texidoc = "By setting @code{voltaOnThisStaff}, repeats can be put
    on more staves in a score."

}
\version "2.1.26"


vmus = \notes { \repeat volta 2 c1 \alternative { d e } } 

\score  {

    \notes \relative c'' <<
	\new StaffGroup <<
	    \context Staff \vmus
	    \new Staff \vmus
	>>
	\new StaffGroup <<
	    \new Staff <<
		\set Staff.voltaOnThisStaff = ##t
		\vmus >>
	    \new Staff \vmus
	>>
    >>

    \paper { raggedright = ##t }
}
