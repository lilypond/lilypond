\version "2.19.21"

\header {
  texidoc="
Setting staff-space to 0 does not cause a segmentation fault.
"
}

\new Staff \with {
  \override StaffSymbol.staff-space = #0
  } {
  \relative {
    c'1 \bar ":|."
  }
}
