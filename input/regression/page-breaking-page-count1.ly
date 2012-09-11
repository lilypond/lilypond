\version "2.16.0"

\header {
  texidoc = "The number of pages in a score can be forced by setting
@code{page-count} in the (book-level) paper block."
}

#(set-default-paper-size "a6")

\book {
  \paper { page-count = 2}
  \score { {c'1 c'1} }
}
