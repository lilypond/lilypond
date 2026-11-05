\version "2.27.5"

\header {
  texidoc = "Beams are quanted on staves whose @code{staff-space} is not 1.
Each beam lies outside its staff, on stems pointing away from it, and is
placed without a @qq{No viable beam quanting found} warning."
}

\layout {
  ragged-right = ##t
}

\new Staff \with {
  \override StaffSymbol.staff-space = #(magstep 4)
} \relative c''' { \voiceOne c8. c16 }

\new Staff \with {
  \override StaffSymbol.staff-space = #7/4
} { \stemUp a''16[ a''16] }

\new Staff \with {
  \magnifyStaff #7/4
} { \grace { a''16 a''16 } e'2 }

\new Staff \with {
  \override StaffSymbol.staff-space = #3
} { \grace { a''16 a''16 } e'2 }
