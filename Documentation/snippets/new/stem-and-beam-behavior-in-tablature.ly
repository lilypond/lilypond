\version "2.13.36"

\header {
  lsrtags = "fretted-strings"
  texidoc = "
The direction of stems is controlled the same way in tablature as in
traditional notation.  Beams can be made horizontal, as shown in this
example.
"
  doctitle = "Stem and beam behavior in tablature"
}

\new TabStaff {
  \relative c {
    \tabFullNotation
    g16 b d g b d g b
    \stemDown
    \override Beam #'concaveness = #10000
    g,,16 b d g b d g b
  }
}
