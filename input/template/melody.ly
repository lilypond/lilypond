\version "2.3.4"

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
