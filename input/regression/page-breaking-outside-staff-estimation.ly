\version "2.16.0"

\header {
  texidoc = "The height-estimation routine takes into account
the fact that the TextScript needs to be moved up to avoid the
note.  This should be spaced on two pages."
}

#(set-default-paper-size "a7")

\book {
  \repeat unfold 5 { g'''1^"Text" \break}
}
