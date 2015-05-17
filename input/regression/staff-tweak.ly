\version "2.19.21"

\header {
  texidoc = "The staff is a grob (graphical object) which may be adjusted as well, for example, to have 6 thick lines and a slightly large @code{staff-space}.
However, beams remain correctly quantized."    

}

\layout  {
  ragged-right = ##t
}


mus =  \relative { c'4 g' d'8 d d d }


<<
  \new Staff {
    \override Staff.StaffSymbol.thickness = #2.0
    \override Staff.StaffSymbol.line-count = #6
    \override Staff.StaffSymbol.staff-space = #1.1
    \mus
  }
  \mus
>>



