\version "2.13.39"

\header {
  texidoc = "Loose lines can specify their padding or min-distance to the
staff for which they don't have affinity."
}

<<
  \new Staff { c'1 }
  \new Lyrics \with {
    \override VerticalAxisGroup
      #'nonstaff-unrelatedstaff-spacing #'padding = #'20
  } \lyricmode { foo }
  \new Staff { c'1 }
>>
