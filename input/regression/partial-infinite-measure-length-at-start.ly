\version "2.25.7"

\header {
  texidoc = "@code{\\partial} produces a warning when used at the start of the
piece when @code{Timing@/.measureLength} is infinite.  In this test, a time
signature remains in effect though the measure length is changed.  This test
should run with expected warnings only."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "cannot calculate a finite measurePosition from an infinite measureLength"))

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    %% This beat structure should make this test sensitive to changes in
    %% handling measure position.  We don't have firm requirements beyond the
    %% warning, but an unintended change in beaming might indicate a problem.
    \time #'(1 3 4 3 1) 8/8
    measureLength = #INF-MOMENT
  }
}

\fixed c' {
  \partial 1.
  c8 8 8 8  8 8 8 8  8 8 8 8
  d1
}
