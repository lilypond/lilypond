\version "2.19.21"

\header {
  texidoc = "
The horizontal position of the beginning of a trill spanner is 
positioned correctly relative to the note head it is attached to,
even if scaled to a smaller size.
"
}

<<
  \new Staff \with {
    fontSize = #-6
    \override StaffSymbol.staff-space = #(magstep -6)
  }
  \relative {
    c'1\startTrillSpan | c\stopTrillSpan |
  }
  \new Staff \relative {
    c'1\startTrillSpan | c\stopTrillSpan |
  }
>>
