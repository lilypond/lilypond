\version "2.3.8"

\header {
  texidoc = "Only a melody."
}

melody =  \relative c' {
  a b c d
}

\score {
  \context Staff \melody
  \paper { }
  \midi  { }
}
