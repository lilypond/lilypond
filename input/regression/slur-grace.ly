\version "2.16.0"

\header {
  texidoc = "Appoggiatura and acciaccaturas use a different slur than the
default, so they produce a nested slur without warnings."
}

\relative c'' {
  c4( \acciaccatura e8 d4 e4 f) |
  c4( \appoggiatura e8 d4 e4 f) |
  c4  \appoggiatura e8 d4 e4 f |
}
