\version "2.13.53"

\header {
  lsrtags = "fretted-strings"
  texidoc = "
Slides for chords can be indicated in both Staff and TabStaff.
String numbers are necessary for TabStaff because automatic
string calculations are different for chords and for single notes,
and @code{\chordGlissando} draws lines between single notes.
"
  doctitle = "Chord glissando in tablature"
}

\include "chord-glissando-init.ly"

myMusic = \relative c' {
  \chordGlissando
  <c\3 e\2 g\1>8 <f\3 a\2 c\1>
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
