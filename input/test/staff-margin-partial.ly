\version "1.3.146"
% test staff margin with partial measure.



\score {
\notes { \property Staff.instrument = "foo" \partial 4 c4 c1 }
  \paper {
    \translator { \StaffContext \consists Instrument_name_engraver }
  }
}
