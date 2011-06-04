\version "2.14.0"

\header {
  texidoc = "System separator positioning works with all spaceable
staff contexts."
}


\paper {
  system-separator-markup = \slashSeparator
  indent = 0
}

\book {
  \new TabStaff {
    c'1
  }

  \new Staff {
    c'1
  }

  \new DrumStaff {
    c'1
  }

  \new RhythmicStaff {
    c'1
  }
}

