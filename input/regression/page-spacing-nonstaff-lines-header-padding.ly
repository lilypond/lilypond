\version "2.17.6"

\header {
  texidoc = "Padding from the header and footer is measured to the
first non-staff line, whether or not it is spaceable."
}

#(set-default-paper-size "a6")

\book {
  \paper {
    top-system-spacing = #'((basic-distance . 1) (padding . 10))
    last-bottom-spacing = #'((basic-distance . 1) (padding . 10))
    annotate-spacing = ##t
    ragged-last-bottom = ##f
  }

  \score {
    <<
      \new Lyrics \with { \override VerticalAxisGroup.staff-affinity = #DOWN } \lyricmode { foo }
      \new Lyrics \with { \override VerticalAxisGroup.staff-affinity = #DOWN } \lyricmode { foo }
      \new Lyrics \with { \override VerticalAxisGroup.staff-affinity = #DOWN } \lyricmode { foo }
      \new Lyrics \with { \override VerticalAxisGroup.staff-affinity = #DOWN } \lyricmode { foo }
      \new Staff { c'1 }
      \new Staff { c'1 }
      \new Lyrics \lyricmode { foo }
      \new Lyrics \lyricmode { foo }
      \new Lyrics \lyricmode { foo }
      \new Lyrics \lyricmode { foo }
    >>
  }
}
