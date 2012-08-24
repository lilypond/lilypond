\version "2.16.0"

\header {
  texidoc = "Phrasing slurs do not collide with tuplet numbers."
}

\relative c'' {
  \voiceOne
  \times 2/3 {
    c8\( b c
  }
  a2.\)
}
