\version "2.16.0"

\header {
  texidoc = "The number of pages in a score can be forced by setting
@code{page-count} in the (book-level) paper block. If there are too
few systems for the number of pages, we append blank pages."
}

#(set-default-paper-size "a6")

\book {
  \paper { page-count = 3}
  \score { {c'1 c'1} }
}
