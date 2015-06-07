\version "2.19.21"

\header {
  texidoc = "Phrasing slurs do not collide with tuplet numbers."
}

\relative {
  \voiceOne
  \tuplet 3/2 {
    c''8\( b c
  }
  a2.\)
}
