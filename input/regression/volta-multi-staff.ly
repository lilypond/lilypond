\header {

    texidoc = "By setting @code{voltaOnThisStaff}, repeats can be put
    also over other staves than the topmost one in a score."

}
\version "2.3.4"


vmus =  { \repeat volta 2 c1 \alternative { d e } } 

\score  {

     \relative c'' <<
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
