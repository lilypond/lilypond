\version "2.13.40"

\header {
  texidoc="
Setting staff-space to 0 does not cause a segmentation fault.
"
}

\new Staff \with {
  \override StaffSymbol #'staff-space = #0
  } {
  \relative c' {
    c1
  }
}
