\version "2.14.0"

\header {
  texidoc = "Spacing rules between Staves coexist with rules affecting
non-staff lines.  Here, the @code{padding} separating items on different
staves is larger than the @code{padding} for associated lyrics."
}

<<
  \new Staff { c''2 c }
  \addlyrics { high \skip2 }
  \new Staff = "bass" \new Voice = "b" { \clef bass c2 c' }
  \new Lyrics \with {
    alignAboveContext = "bass"
  } \lyricsto "b" \lyricmode { bass \skip2 }
>>
\layout {
  \context {
    \Lyrics
    \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . 0.2))
  } \context {
    \Staff
    \override VerticalAxisGroup #'default-staff-staff-spacing = #'((padding . 4))
  }
}
