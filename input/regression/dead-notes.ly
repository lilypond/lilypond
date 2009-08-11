\version "2.13.4"

\header{ texidoc = "Muted notes (also called dead notes) are supported
                    within normal staves and tablature."
       }

mynotes = \relative c,, {
   \deadNotesOn
   e8. e16
   \deadNotesOff
   g4 a b |
   e8. \deadNote e16 g4 a b |
   e,4. \deadNote { e8 e e } e4 |
   < e, \deadNote b' e >8 < e \deadNote b' e > < e \deadNote b' e >4 < e \deadNote b' e >4 r
   \bar "|."
}

\context StaffGroup <<
  \context Staff {
    \context Voice {  % Warning: explicit voice instantiation is required
                      %   to have deadNotesOff work properly
                      %   when deadNotesOn comes at the beginning
                      %   of the piece
      \clef "bass_8"
      \mynotes
    }
  }
  \context TabStaff {
    \context TabVoice {  % Warning:  explicit voice instantiation is
                         %   required to have deadNotesOff work properly
                         %   when deadNotesOn comes at the beginning
                         %   of the piece
      \set TabStaff.stringTunings = #bass-tuning
      \mynotes
    }
  }
>>


