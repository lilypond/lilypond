\version "2.23.5"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond issues warnings for conflicting simultaneous
marks and engraves only the first -- in this case, 1, 2, and@tie{}3."
}

\layout {
  \context {
    \Score
    markFormatter = #format-mark-numbers
  }
}

\parallelMusic upper,lower {
  R1 \mark 1    |
  R1 \mark "A!" |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "rehearsal-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "ad-hoc-mark-event")

  R1 \mark 2        |
  R1 \mark \default |
  %% no warning

  R1 \mark 3 |
  R1 \mark 9 |
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "conflict with event: `%s'") "rehearsal-mark-event")
  #(ly:expect-warning (ly:translate-cpp-warning-scheme
                       "discarding event: `%s'") "rehearsal-mark-event")

  R1 |
  R1 |
}

\new Staff <<
  \new Voice \upper
  \new Voice \lower
>>
