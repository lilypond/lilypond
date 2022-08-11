\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings when @code{\\codaMark \\default}
conflicts with certain other simultaneous marks, and engraves only the
first.

Coda marks 1 to@tie{}3 should appear with various rehearsal marks.
Coda marks 4 to@tie{}6, 8, and@tie{}9 should appear alone."
}

\layout {
  \context {
    \Score
    rehearsalMarkFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \codaMark \default |
  R1 \mark "A!" |

  R1 \codaMark \default |
  R1 \mark \default |

  R1 \codaMark \default |
  R1 \mark 9 |

  R1 \codaMark \default |
  R1 \segnoMark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "coda-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 \codaMark \default |
  R1 \segnoMark 9 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "coda-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 \codaMark \default |
  R1 \codaMark \default |
  %% no warning

  R1 \codaMark \default |
  R1 \codaMark 8 |
  %% no warning

  R1 \codaMark \default |
  R1 \sectionLabel "Coda" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "coda-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "section-label-event")

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
