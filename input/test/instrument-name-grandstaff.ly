\version "1.3.146"


\score {
  \notes
  \notes \context GrandStaff <
    \context Staff = treble    {
      \property GrandStaff.instrument = "Violini  "
      \property Staff.instrument = " vn I" { c''4 }}
    \context Staff = bass { \property Staff.instrument = " vn II" c''4 }>


\paper {
linewidth=-1.0
\translator { \StaffContext
  \consists "Instrument_name_engraver"
  }
\translator { \GrandStaffContext \consists "Instrument_name_engraver" }
}}
