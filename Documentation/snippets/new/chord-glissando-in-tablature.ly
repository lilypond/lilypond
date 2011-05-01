\version "2.13.53"

\header {
  lsrtags = "fretted-strings"
  texidoc = "
Slides for chords can be indicated in both Staff and TabStaff.
String numbers are necessary for TabStaff because automatic
string calculations are different for chords and for single notes.
"
  doctitle = "Chord glissando in tablature"
}

myMusic = \relative c' {
  <c\3 e\2 g\1>1 \glissando <f\3 a\2 c\1>
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \myMusic
    }
    \new TabStaff {
      \myMusic
    }
  >>
}
