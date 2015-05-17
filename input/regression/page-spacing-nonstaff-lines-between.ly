\version "2.19.21"

\header {
  texidoc = "Non-staff lines between two systems don't confuse the
layout engine.  In particular, they don't interfere with
@code{system-system-spacing}, which controls the flexible spacing
between the two closest staves of consecutive systems."
}

\paper {
  ragged-right = ##t
  system-system-spacing = #'((basic-distance . 20) (minimum-distance . 30))
  annotate-spacing = ##t
}

<<
  \new Lyrics
  \with { \override VerticalAxisGroup.staff-affinity = #DOWN }
  \lyricmode {
    My2 first Li4 -- ly song,2
    My2 first Li4 -- ly song,2
  }
  \context Voice = "voice" \relative {
    d''2 d c4 bes a2 \break
    d'2 d c4 bes a2
  }
  \new Lyrics \lyricmode {
    Not2 much can4 go wrong!2
    Not2 much can4 go wrong!2
  }
>>
