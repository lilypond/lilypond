\version "2.25.35"

\header {
  texidoc = "The number of pages in a score can be forced by setting
@code{page-count} in the (book-level) paper block. Even if there are
too many systems for that number of pages, we will squeeze them in."
}

#(set-default-paper-size "a6")

\book {
  \paper { page-count = 1}
  \score { { \*10 { c'1 \break } } }
}
