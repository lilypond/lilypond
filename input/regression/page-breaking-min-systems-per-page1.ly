\version "2.16.0"

#(set-default-paper-size "a6")

\header {
  texidoc = "The min-systems-per-page variable forces each page to have
a minimum number of systems. Titles do not count as systems here."
  title = "Title"
}

\book {
  \paper {
    min-systems-per-page = 5
  }

  { \repeat unfold 11 { c'1 } \pageBreak \repeat unfold 6 { c'1 } }
}
