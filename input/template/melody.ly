\version "1.5.68"

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
