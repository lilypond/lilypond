\header {
    texidoc = "A loose line (eg. a lyric line) at the top of a system
gets spaced as though it wasn't loose."
}

\layout {
  ragged-right = ##t
  \context {
    \Lyrics
    \override VerticalAxisGroup #'inter-loose-line-spacing #'space = #20
    \override VerticalAxisGroup #'staff-affinity = #DOWN
  }
}
<<
  \new Lyrics \lyricmode {
    My2 first Li4 -- ly song,
  }
  \new Lyrics \lyricmode {
    Not2 much can4 go wrong!
  }
  \context Voice = "voice" \relative {
    d'2 d c4 bes a2
  }
>>

\version "2.12.0"
