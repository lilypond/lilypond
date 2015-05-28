\version "2.19.21"

\header {
  texidoc = "
  Instrument names (aligned on axis group spanners) ignore dynamic and
  pedal line spanners."
}

\paper {
  line-width = 3\cm
  indent = 0\cm
}

\relative {
  \set Staff.shortInstrumentName = "foo"
  f'1 | f1 | f1 |
  f1\< | f1 | f1\! |
}

% EOF
