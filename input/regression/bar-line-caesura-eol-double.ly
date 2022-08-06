\version "2.23.12"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When @code{caesuraType} is set appropriately,
@code{\\caesura} inserts a double bar visible only at line break, with
priority less than a measure bar.

These notes should be followed by these bar lines: D, none; E, single;
F, dotted; G, single; A, double; B, double."
}

\layout {
  \context {
    \Score
    caesuraType = #'((underlying-bar-line . "x-||"))
    underlyingRepeatBarType = "!" % avoid "||" for clarity
  }
}

\include "bar-line-caesura-underlying-test.ily"
