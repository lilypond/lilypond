\version "2.17.11"

\header {
  texidoc = "Phrasing slurs do not collide with tuplet numbers."
}

\relative c'' {
  \voiceOne
  \tuplet 3/2 {
    c8\( b c
  }
  a2.\)
}
