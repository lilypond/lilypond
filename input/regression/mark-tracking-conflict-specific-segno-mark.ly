\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings when @code{\\segnoMark @var{n}}
conflicts with certain other simultaneous marks, and engraves only the
first.

Segno marks 1 to@tie{}3 should appear with various rehearsal marks.
Segno marks 4 to@tie{}8 should appear alone."
}

\layout {
  \context {
    \Score
    rehearsalMarkFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \segnoMark 1 |
  R1 \mark "A!" |
  %% no warning

  R1 \segnoMark 2 |
  R1 \mark \default |
  %% no warning

  R1 \segnoMark 3 |
  R1 \mark 3 |
  %% no warning

  R1 \segnoMark 4 |
  R1 \segnoMark \default |
  %% no warning

  R1 \segnoMark 5 |
  R1 \segnoMark 9 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "segno-mark-event")

  R1 \segnoMark 6 |
  R1 \codaMark \default |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "coda-mark-event")

  R1 \segnoMark 7 |
  R1 \codaMark 3 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "segno-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "coda-mark-event")

  R1 \segnoMark 8 |
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
