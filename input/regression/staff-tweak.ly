\version "2.1.7"

\header {
texidoc = "The staff is a grob, and may be adjusted as well: this one
 shows a staff with 6 thick line, and a slightly large staffspace.
Beams remain correctly quantized."    

}


mus = \notes \relative c' { c4 g' d'8 d d d }

\score {
  \notes <<
    \new Staff {
	\property
	Staff.	StaffSymbol \set #'thickness = #2.0
	\property
	Staff.StaffSymbol \set #'line-count = #6
	\property
	Staff.StaffSymbol \set #'staff-space = #1.1
	\mus
    }
    \mus
  >>
  \paper  {
    raggedright = ##t
  }
}

