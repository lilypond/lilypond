\version "2.19.21"

\header {
  texidoc = "Appoggiatura and acciaccaturas use a different slur than the
default, so they produce a nested slur without warnings."
}

\relative {
  c''4( \acciaccatura e8 d4 e4 f) |
  c4( \appoggiatura e8 d4 e4 f) |
  c4  \appoggiatura e8 d4 e4 f |
}
