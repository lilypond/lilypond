\version "2.19.21"
\header {
  texidoc = "Beam collisions are resistant to scaled down staves."
}

\new Staff \with {
  fontSize = #-3
  \override StaffSymbol.staff-space = #(magstep -3)
  \override StaffSymbol.thickness = #(magstep -3) }
<<
  \relative { e''16[ f] }
  \\
  \relative { \autoBeamOff g'' b }
>>
