#(ly:set-option 'old-relative)
\header {

    texidoc = "By setting @code{voltaOnThisStaff}, repeats can be put on more staffs in a score."

}
\version "1.9.4"


vmus = \notes { \repeat volta 2 c1 \alternative { d e } } 

\score  {

    \notes \relative c'' <<
	\new StaffGroup <<
	    \context Staff \vmus
	    \new Staff \vmus
	>>
	\new StaffGroup <<
	    \new Staff <<
		\property Staff.voltaOnThisStaff = ##t
		\vmus >>
	    \new Staff \vmus
	>>
    >>

    \paper { raggedright = ##t }
}
