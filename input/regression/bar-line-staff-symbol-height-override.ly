\version "2.23.5"

\header {
  texidoc = "Bar lines account for user tweaks to staff symbol height."
}

\new Staff \with {
  \override StaffSymbol.Y-extent = #'(-5 . 4)
}
{ c'1 }
