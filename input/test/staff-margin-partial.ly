% test staff margin with partial measure.
\score {
\notes { \property Staff.instrument = "foo" \partial 4; c4 c1 }
\paper { \translator { \StaffContext \consists Staff_margin_engraver; }}
}
