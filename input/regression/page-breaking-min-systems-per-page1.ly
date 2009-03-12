\version "2.13.1"

#(set-default-paper-size "a6")

\header {
  texidoc = "The min-systems-per-page variable forces each page to have
a minimum number of systems. Titles do not count as systems here.

This exposes a bug with the current implementation; min-systems-per-page
is not honored if to do so we would need uneven line breaking."
  title = "Example"
}

\paper {
  min-systems-per-page = 5
}

{ \repeat unfold 11 { c'1 } \pageBreak \repeat unfold 6 { c'1 } }