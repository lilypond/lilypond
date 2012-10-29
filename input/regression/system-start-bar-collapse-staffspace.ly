\version "2.17.6"

\header {
  texidoc = "When the staff-space is increased, the system-start delimiter
should still be collapsed (i.e. the collapse-height should not give an absolute
length, but a multiple of staff-spaces)."
}

\new Staff \with { \override StaffSymbol.staff-space = #1.4 }
{
  a4 b c d 
}
