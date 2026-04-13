\version "2.25.35"

\header {
  texidoc = "The height-estimation routine doesn't get confused
by multiple outside-staff grobs in the same measure."
}

#(set-default-paper-size "a7")

\book {
  \*4 { \*4 { g'''4^"Text" } \break }
}
