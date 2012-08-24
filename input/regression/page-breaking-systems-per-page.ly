\version "2.16.0"

\header {
  texidoc = "The systems-per-page variable forces a certain number of systems
per page. Titles are not counted as systems."
  title = "Title"
}

#(set-default-paper-size "a6")

\book {
  \paper {
    systems-per-page = 3
  }

  { \repeat unfold 3 { c'1 } \pageBreak \repeat unfold 3 { c'1 } }
}
