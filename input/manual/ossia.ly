
\header { texidoc = "Ossia fragments can be done with starting and
stopping staves. " }

\version "2.10.0"
\paper { ragged-right = ##t }

<<
  \new Staff \with
  {
    \remove "Time_signature_engraver"
    fontSize = #-2
    \override StaffSymbol #'staff-space = #(magstep -2)
    firstClef = ##f
  }
  \relative c'' {
    \stopStaff
    \skip 2

    \startStaff
    \clef treble
    bes8[^"ossia" g bes g]
    \stopStaff

    s2

    \startStaff
    f8 d g4 
  }
  \new Staff  \relative
  {
    \time 2/4
    c4 c g' g a a g2
  }

>>
