\version "1.9.1"
\header { texidoc = "@cindex Instrument Name Grandstaff
You can name the whole grandstaff in addition to individual staffs. " }

\score {
  \notes
  \notes \context GrandStaff <
    \context Staff = treble    {
      \property GrandStaff.instrument = "Violini  "
      \property Staff.instrument = " vn I" { c''4 }}
    \context Staff = bass { \property Staff.instrument = " vn II" c''4 }>


\paper {
raggedright = ##t
\translator { \StaffContext
  \consists "Instrument_name_engraver"
  }
\translator { \GrandStaffContext \consists "Instrument_name_engraver" }
}}

