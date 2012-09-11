\version "2.16.0"

\header {
  texidoc = "The height-estimation routine doesn't get confused
by multiple outside-staff grobs in the same measure."
}

#(set-default-paper-size "a7")

\book {
  \repeat unfold 4 { \repeat unfold 4 {g'''4^"Text"} \break}
}
