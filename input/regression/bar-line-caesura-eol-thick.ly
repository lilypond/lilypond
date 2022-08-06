\version "2.23.12"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When @code{caesuraType} is set appropriately,
@code{\\caesura} inserts a thick bar line that is visible only at a
line break, with priority less than a measure bar.

These notes should be followed by these bar lines: D, none; E, single;
F, double; G, single; A, thick; B, thick."
}

\layout {
  \context {
    \Score
    caesuraType = #'((underlying-bar-line . "x-."))
  }
}

\include "bar-line-caesura-underlying-test.ily"
