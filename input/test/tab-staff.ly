\version "1.5.68"



%{

Kludge for half-assed tab notation (you need to fill the numbers
yourself.)

%}

bla =  \notes \relative c' { <c4 e g>  <c4. e g> <c8 f a> <c4 f a> <c1 e g> } 

\score { \notes
<
  \context Voice = BLA \bla
  \context TabStaff \context Thread \bla
>

\paper {
 \translator {
  \StaffContext
  \name TabStaff
  StaffSymbol \override #'line-count = #6
  \remove "Clef_engraver"
  \remove "Time_signature_engraver"
  \consists "Pitch_squash_engraver"
   NoteHead \override #'transparent = ##t
   Stem \override #'transparent = ##t
   NoteHead \override #'staff-position = #-6
   Dots \override #'transparent = ##t
   squashedPosition = #-4
   Stem \override #'length = #12
}
\translator {
\ScoreContext
\accepts TabStaff
}
}
}
