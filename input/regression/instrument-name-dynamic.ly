\version "2.16.0"

\header {
  texidoc = "
  Instrument names (aligned on axis group spanners) ignore dynamic and
  pedal line spanners."
}

\paper {
  line-width = 3\cm
  indent = 0\cm
}

\relative c' {
  \set Staff.shortInstrumentName = "foo"
  f1 | f1 | f1 |
  f1\< | f1 | f1\! |
}

% EOF
