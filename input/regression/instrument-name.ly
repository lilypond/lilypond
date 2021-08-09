\version "2.16.0"
\header{
  texidoc="
Staff margins are also markings attached to bar lines.  They should be
left of the staff, and be centered vertically with respect to the staff.  
They may be on normal staves, but also on compound staves, like the 
PianoStaff.
"
}

\layout {
  ragged-right = ##t
}






\new StaffGroup <<
  \context PianoStaff <<
    \new Staff    {
      \set PianoStaff.instrumentName = "Piano"
      \set Staff.instrumentName = "Right" { c''4 }}
    \new Staff {
      \set Staff.instrumentName = "Left"
      \clef bass c4
    }
  >>

  \lyrics {
    \set vocalName = "bert"
    blah
  }
  \new Staff { c''4 } 
>>
