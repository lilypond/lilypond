\version "2.19.21"

\header {
  texidoc = "
Context property @code{defaultStrings} defines desired strings
for fret calculations if no strings are defined explicitly.
"
}

mymusic = \relative {
  <c c'>4
  \set defaultStrings = #'(5 3)
  <c c'>4
  <d d'>4
  <e e'>4
  <c c'>2\6\4
  \unset defaultStrings
  <c c'>2
}

\score {
  <<
    \new Staff {
      \clef "treble_8"
      \mymusic
    }
    \new TabStaff {
      \mymusic
    }
  >>
}
