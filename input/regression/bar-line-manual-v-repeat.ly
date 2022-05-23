\version "2.23.10"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\bar} can override repeat bar lines.  The first
system should end with no bar line.  The second system should begin
with no bar line and end with a measure bar line."
}

\layout {
  indent = 0
  ragged-right = ##t
}

\new Staff \fixed c' {
  R1
  \bar "" \break
  \repeat volta 2 R1
  \bar "|"
}
