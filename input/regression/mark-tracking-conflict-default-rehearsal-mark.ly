\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings when @code{\\mark \\default}
conflicts with certain other simultaneous marks, and engraves only the
first.

Rehearsal marks 1, 2, and@tie{}4 should appear alone.  Rehearsal marks
5 to@tie{}9 should appear with various performance marks."
}

\layout {
  \context {
    \Score
    rehearsalMarkFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \mark \default |
  R1 \mark "A!"     |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "rehearsal-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "ad-hoc-mark-event")

  R1 \mark \default |
  R1 \mark \default |
  %% no warning

  R1 \mark \default |
  R1 \mark 4        |
  %% no warning

  R1 \mark \default |
  R1 \segnoMark \default |
  %% no warning

  R1 \mark \default |
  R1 \segnoMark 2 |
  %% no warning

  R1 \mark \default |
  R1 \codaMark \default |
  %% no warning

  R1 \mark \default |
  R1 \codaMark 2 |
  %% no warning

  R1 \mark \default |
  R1 \sectionLabel "Coda" |
  %% no warning

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
