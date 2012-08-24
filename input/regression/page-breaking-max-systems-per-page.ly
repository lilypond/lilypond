\version "2.16.0"

\header {
  texidoc = "The max-systems-per-page variable prevents more
than a given number of systems from being on a page. Titles
are not counted as systems. \\noPageBreak can override
max-systems-per-page in unusual situations."
  title = "Title"
}

#(set-default-paper-size "a6")

\book {
  \paper {
    max-systems-per-page = 1
  }

  { c'1 \break c'1 \break \noPageBreak c'1 \break c'1 }
}
