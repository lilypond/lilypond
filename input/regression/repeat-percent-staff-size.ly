\version "2.23.9"

\header {
  texidoc = "Slash and percent signs are correctly scaled at different
staff sizes."
}

mus = {
  \repeat percent 2 { c'8 8 8 8 }
  \repeat percent 2 { c'4 8 8 }
  \repeat percent 2 { c'1 }
  \repeat percent 2 { c'1 1 }
}

\new Staff \with { \magnifyStaff #1/2 } \mus
\new Staff \mus
\new Staff \with { \magnifyStaff #3/2 } \mus
