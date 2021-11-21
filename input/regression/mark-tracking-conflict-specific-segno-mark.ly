\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings for conflicting simultaneous
marks and engraves only the first -- in this case, segno marks 1 to 5
in order."
}

\layout {
  \context {
    \Score
    markFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \segnoMark 1 |
  R1 \mark "A!" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "ad-hoc-mark-event")

  R1 \segnoMark 2 |
  R1 \mark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \segnoMark 3 |
  R1 \mark 3 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \segnoMark 4 |
  R1 \segnoMark \default |
  %% no warning

  R1 \segnoMark 5 |
  R1 \segnoMark 9 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
