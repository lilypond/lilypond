\version "2.13.4"

\header {
  lsrtags = "fretted-strings"
  texidoc = "
A hammer in tablature can be faked with slurs.
"
  doctitle = "Faking a hammer in tablatures"
}

\score {
  \new TabStaff {
    \relative c'' {
      \tabFullNotation
      c4( d) d( d)
      d2( c)
    }
  }
}
