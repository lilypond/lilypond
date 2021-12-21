\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header  {
  texidoc = "@code{Mark_@/tracking_@/translators} operate
independently in independent contexts.  The upper staff has marks 1,
2, and@tie{}3.  The lower staff has marks 2, 3, and 1 at the same
points."
}

\layout {
  \context {
    \Score
    \remove "Mark_engraver"
    \remove "Mark_tracking_translator"
    \remove "Staff_collecting_engraver"
    rehearsalMarkFormatter = #format-mark-numbers
  }
  \context {
    \Staff
    \consists "Mark_engraver"
    \consists "Mark_tracking_translator"
    \consists "Staff_collecting_engraver"
  }
}

\score {
  <<
    \new Staff { R1 \mark 1 R1 \mark \default R1 \mark 3 R1 }
    \new Staff { R1 \mark 2 R1 \mark \default R1 \mark 1 R1 }
  >>
}
