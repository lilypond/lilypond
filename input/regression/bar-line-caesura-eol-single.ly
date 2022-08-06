\version "2.23.12"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When @code{caesuraType} is set appropriately,
@code{\\caesura} inserts a bar line that is visible only at a line
break.

These notes should be followed by these bar lines: D, none; E, single;
F, double; G, single; A, single; B, single."
}

\layout {
  \context {
    \Score
    %% Because this looks the same as a measure bar line, priority makes
    %% no difference.  bar-line would work just as well.
    caesuraType = #'((underlying-bar-line . "x-|"))
  }
}

\include "bar-line-caesura-underlying-test.ily"
