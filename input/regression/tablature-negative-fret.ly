\version "2.14.0"

\header {

  texidoc = "
Negative fret numbers calculated due to assigning a string number
can be displayed, ignored, or recalculated.  Here we should have
all three cases demonstrated.
"

}

myMusic = \relative c'  {
  <c\1>1 ^\markup { recalculate }
  \set TabStaff.handleNegativeFrets = #'include
  <c\1>1 ^ \markup { include }
  \set TabStaff.handleNegativeFrets = #'ignore
  <c\1>1 ^ \markup { ignore }
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \textLengthOn
      \myMusic
    }
    \new TabStaff {
      \myMusic
    }
  >>
}
