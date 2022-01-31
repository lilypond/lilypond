\version "2.23.7"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="@code{\\bar \"|\"} can override a repeat bar line.  The
line should end with a measure bar line."
}

\new Staff \fixed c' {
  \repeat volta 2 R1 \bar "|"
}
