% test staff margin with partial measure.

\version "1.3.42";

\score {
\notes { \property Staff.instrument = "foo" \partial 4; c4 c1 }
  \paper {
    \translator { \StaffContext \consists Instrument_name_engraver; }
  }
}
