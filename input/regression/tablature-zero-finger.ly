\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "No open string for pitch ~a") "#<Pitch f >")

\header {
  texidoc="
A fingering indication of zero counts as an open string for fret
calculations.  An inappropriate request for an open string will generate
a warning message and set the requested pitch in the tablature.
"
}

mymusic = \relative {
  \set minimumFret = #1
  <d-0 d'-2 f-3 a-1>1
  <f-0>
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
