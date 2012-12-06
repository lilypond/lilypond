\version "2.17.6"

\header {
  texidoc = "Non-staff lines (such as @code{Lyrics}) can specify
their @code{padding} or @code{minimum-distance} to the staff for
which they don't have affinity."
}

<<
  \new Staff { c'1 }
  \new Lyrics \with {
    \override VerticalAxisGroup.nonstaff-unrelatedstaff-spacing.padding = #'20
  } \lyricmode { foo }
  \new Staff { c'1 }
>>
