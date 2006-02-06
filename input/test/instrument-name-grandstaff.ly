\version "2.7.32"
\header { texidoc = "@cindex Instrument Name Grandstaff
You can have a name for the whole @code{GrandStaff} in addition to 
individual @code{Staff}s. " }

\score {
  
   \context GrandStaff <<
    \context Staff = "treble"    {
      \set GrandStaff.instrument = "Violini  "
      \set Staff.instrument = " vn I" { c''4 }}
    \context Staff = "bass" { \set Staff.instrument = " vn II" c''4 }>>


\layout {
ragged-right = ##t
\context { \GrandStaff \consists "Instrument_name_engraver" }
}}

