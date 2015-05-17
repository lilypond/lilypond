\version "2.19.21"

\header {

  texidoc = "
With full notation, the dots on the tablature heads should
respect two-digit fret numbers.
"

}

myMusic = \relative  {
  <f'\3 a c>4.
  <f\3 g d'>4.
  <f\3 a d>4
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \myMusic
    }
    \new TabStaff {
      \tabFullNotation
      \myMusic
    }
  >>
}
