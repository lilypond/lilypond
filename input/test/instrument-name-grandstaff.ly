\version "2.1.22"
\header { texidoc = "@cindex Instrument Name Grandstaff
You can name the whole grandstaff in addition to individual staffs. " }

\score {
  \notes
  \notes \context GrandStaff <<
    \context Staff = treble    {
      \set GrandStaff.instrument =  "Violini  "
      \set Staff.instrument =  " vn I" { c''4 }}
    \context Staff = bass { \set Staff.instrument =  " vn II" c''4 }>>


\paper {
raggedright = ##t
\translator { \GrandStaffContext \consists "Instrument_name_engraver" }
}}

