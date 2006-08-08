\version "2.9.13"
\header { texidoc = "@cindex Instrument Name Grandstaff
You can have a name for the whole @code{GrandStaff} in addition to 
individual @code{Staff}s. " }

\score {
  
   \new GrandStaff <<
    \new Staff =  "treble"    {
      \set GrandStaff.instrumentName = "Violini  "
      \set Staff.instrumentName = " vn I" { c''4 }}
    \new Staff =  "bass" { \set Staff.instrumentName = " vn II" c''4 }>>


\layout {
ragged-right = ##t
\context { \GrandStaff \consists "Instrument_name_engraver" }
}}

