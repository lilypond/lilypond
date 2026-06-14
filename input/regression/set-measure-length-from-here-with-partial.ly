\version "2.27.2"

\header {
  texidoc = "When @code{\\setMeasureLengthFromHere} and @code{\\partial} are
used in the same time step, the new position is set relative to the new length.
Measures@tie{}1 and@tie{}3 should be beamed as @code{3,2,2}."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    \overrideTimeSignatureSettings 6/8 1/8 3,1,1,1,2,2 #'()
  }
}

music = \fixed c' {
  \time 6/8
  \partial 8*3
  d8 d8 d8 |
  e8 e8 e8 % measurePosition is 8*3
  %% extend this measure to 8*10 (3+7), but skip 8*3 (7-4) in the middle
  \partial 8*4 \setMeasureLengthFromHere 8*7
  \contextPropertyCheck Timing.measureLength #10/8
  e8 e8 e8 e8 |
  \setDefaultMeasureLength
  \contextPropertyCheck Timing.measureLength #6/8
  f8 f8 f8 f8 f8 f8 |
  g8 g8 g8 % measurePosition is 8*3
  %% extend this measure to 8*10 (3+7), but skip 8*3 (7-4) in the middle
  \setMeasureLengthFromHere 8*7 \partial 8*4
  \contextPropertyCheck Timing.measureLength #10/8
  g8 g8 g8 g8 |
  \setDefaultMeasureLength
  \contextPropertyCheck Timing.measureLength #6/8
}

\new Score { \music }
