\version "1.9.4"

\header {
  texidoc = "Only a melody."
}

melody = \notes \relative c' {
  a b c d
}

\score {
  \context Staff \melody
  \paper { }
  \midi  { }
}
