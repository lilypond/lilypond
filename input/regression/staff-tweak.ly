\version "2.1.22"

\header {
texidoc = "The staff is a grob, and may be adjusted as well: this one
 shows a staff with 6 thick line, and a slightly large staffspace.
Beams remain correctly quantized."    

}


mus = \notes \relative c' { c4 g' d'8 d d d }

\score {
  \notes <<
    \new Staff {
	\override Staff.StaffSymbol  #'thickness = #2.0
	\override Staff.StaffSymbol  #'line-count = #6
	\override Staff.StaffSymbol  #'staff-space = #1.1
	\mus
    }
    \mus
  >>
  \paper  {
    raggedright = ##t
  }
}

