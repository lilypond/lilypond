\version "2.23.12"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="When @code{caesuraType} is set appropriately,
@code{\\caesura} inserts a double bar line with priority higher than a
measure bar line and lower than a section bar line.

These notes should be followed by these bar lines: D, double; E, double;
F, double; G, double; A, thick."
}

\layout {
  indent = 0
  ragged-right = ##t
}

staff = \new Staff \fixed c' {
  %% BOL alone
  \caesura

  %% mid-line alone
  d2 \caesura

  %% mid-line with measure bar
  e2 \caesura |

  %% EOL alone
  f2 \caesura \break
  r2 |

  %% mid-line with underlying repeat bar
  g2 \caesura \codaMark 1
  r2 |

  %% EOL with section bar
  a2 \caesura \section
}

\new Score \with {
  caesuraType = #'((bar-line . "||"))
  sectionBarType = "."
  underlyingRepeatBarType = "!"
} {
  \new PianoStaff << \staff \staff >>
}
