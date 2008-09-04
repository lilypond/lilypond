\version "2.11.58"

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
