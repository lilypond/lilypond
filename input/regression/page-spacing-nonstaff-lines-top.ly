\version "2.19.21"

\header {
  texidoc = "A non-staff line (such as @code{Lyrics}) at the top
of a system is spaced appropriately."
}

\layout {
  ragged-right = ##t
  \context {
    \Lyrics
    \override VerticalAxisGroup.nonstaff-nonstaff-spacing.minimum-distance = #20
    \override VerticalAxisGroup.staff-affinity = #DOWN
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
    d''2 d c4 bes a2
  }
>>

