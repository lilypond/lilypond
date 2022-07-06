\version "2.23.11"

\header {
  texidoc = "Settings can be applied to @code{\\markup \\rhythm},
either using music commands in the music argument, or using
a @code{\\layout} block."
}

\layout {
  \context {
    \StandaloneRhythmScore
    \override SpacingSpanner.spacing-increment = 4
  }
  \context {
    \StandaloneRhythmStaff
    \override StaffSymbol.line-count = 2
  }
  \context {
    \StandaloneRhythmVoice
    \tieDashed
  }
}

\markup \rhythm {
  \stemDown
  16~ 16 \xNote 8 \tupletUp \tuplet 3/2 { 8 8 8 }
}
