\version "1.9.8"
\header {
texidoc = "The staff is a grob, and may be adjusted as well: this one
 shows a staff with 6 thick line, and a slightly large staffspace.
Beams remain correctly quantized."    
}


mus = \notes \relative c' { c4 g' d'8 d d d }

\score {
  \notes <<
    \new Staff \mus
    \new Staff \with { 
	StaffSymbol \set #'thickness = #2.0
	StaffSymbol \set #'line-count = #6
	StaffSymbol \set #'staff-space = #1.1
    } \mus
  >>
  \paper  {
    raggedright = ##t
  } 
}

