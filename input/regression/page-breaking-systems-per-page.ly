\version "2.25.35"

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

  { \*3 c'1 \pageBreak \*3 c'1 }
}
