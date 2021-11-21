\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings for conflicting simultaneous
marks and engraves only the first -- in this case, 1! to 5! in order."
}

\layout {
  \context {
    \Score
    markFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \mark "1!" |
  R1 \mark "A!" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "ad-hoc-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "ad-hoc-mark-event")

  R1 \mark "2!"     |
  R1 \mark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "ad-hoc-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \mark "3!" |
  R1 \mark 3    |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "ad-hoc-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \mark "4!"     |
  R1 \segnoMark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "ad-hoc-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 \mark "5!" |
  R1 \segnoMark 5 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "ad-hoc-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
