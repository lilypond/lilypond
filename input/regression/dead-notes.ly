\version "2.13.4"

\header{ texidoc = "Muted notes (also called dead notes) are supported
                    within normal staves and tablature."
       }

deadnotes = \relative c,, {
   e8. \deadNotesOn e16 \deadNotesOff g4 a b |
   e8. \deadNote e16 g4 a b |
   e,4. \deadNote { e8 e e } e4 |
   < e, \deadNote b' e >8 < e \deadNote b' e > < e \deadNote b' e >4 < e \deadNote b' e >4 r
   \bar "|."
}

\context StaffGroup <<
  \context Staff <<
    \clef "bass_8"
    \deadnotes
  >>
  \context TabStaff <<
    \set TabStaff.stringTunings = #bass-tuning
    \deadnotes
  >>
>>


