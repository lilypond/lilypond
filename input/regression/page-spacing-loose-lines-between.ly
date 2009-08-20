\version "2.13.4"

\header {
  texidoc = "Loose lines between two systems don't confuse the layout engine.
In particular, they don't interfere with @var{between-system-spacing},
which measures distances between spaceable staves."
}

\paper {
  ragged-right = ##t
  between-system-spacing = #'((space . 20) (minimum-distance . 30))
  annotate-spacing = ##t
}

<<
  \new Lyrics
  \with { \override VerticalAxisGroup #'staff-affinity = #DOWN }
  \lyricmode {
    My2 first Li4 -- ly song,2
    My2 first Li4 -- ly song,2
  }
  \context Voice = "voice" \relative {
    d'2 d c4 bes a2 \break
    d'2 d c4 bes a2
  }
  \new Lyrics \lyricmode {
    Not2 much can4 go wrong!2
    Not2 much can4 go wrong!2
  }
>>
