\version "2.25.7"

\header {
  texidoc = "@code{\\partial} produces a warning when used in mid piece when
@code{Timing@/.measureLength} is infinite.  In this test, a time signature
remains in effect though the measure length is changed.  This test should run
with expected warnings only."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning
  (G_ "cannot calculate a finite measurePosition from an infinite measureLength"))

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
    measureLength = #INF-MOMENT
  }
}

{
  c'1
  \partial \breve
  d'\breve
  e'1
}
