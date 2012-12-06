\version "2.17.6"

\header {
  texidoc = "Metronome marks aligned on notes do not interfere with
the positioning of loose columns in other staves.  Here the loose
column supporting the clef is correctly placed immediately before
the second note in the lower staff."
}

\score {
  <<
    \new Staff \relative c' {
      c8 c c c
      \tempo 4 = 60
      c2
    }
    \new Staff \relative c' {
      c2 \clef bass c2
    }
  >>
  \layout {
    \context {
      \Score
      \override NonMusicalPaperColumn.stencil = #ly:paper-column::print
    }
  }
}
