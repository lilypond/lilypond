\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings for conflicting simultaneous
marks and engraves only the first -- in this case, segno marks 1
to@tie{}4, then 9 to@tie{}12."
}

\layout {
  \context {
    \Score
    rehearsalMarkFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \segnoMark \default |
  R1 \mark "A!" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "ad-hoc-mark-event")

  R1 \segnoMark \default |
  R1 \mark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \segnoMark \default |
  R1 \mark 9 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 \segnoMark \default |
  R1 \segnoMark \default |
  %% no warning

  R1 \segnoMark \default |
  R1 \segnoMark 9 |
  %% no warning

  R1 \segnoMark \default |
  R1 \codaMark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "coda-mark-event")

  R1 \segnoMark \default |
  R1 \codaMark 2 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "coda-mark-event")

  R1 \segnoMark \default |
  R1 \sectionLabel "Coda" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "section-label-event")

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
