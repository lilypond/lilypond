%{

Kludge for half-assed tab notation (you need to fill the numbers
yourself.)

%}

bla = \notes \relative c' { <c4 e g>  <c4. e g> <c8 f a> <c4 f a> <c1 e g> } 

\score { \notes
<
  \context Voice = BLA \bla
  \context TabStaff \context Thread \bla
>

\paper {
 \translator {
  \StaffContext
  \name TabStaff;
  basicStaffSymbolProperties \push #'line-count = #6
  \remove "Clef_engraver";
  \remove "Time_signature_engraver";
  \consists "Pitch_squash_engraver";
   basicNoteHeadProperties \push #'transparent = ##t
   basicStemProperties \push #'transparent = ##t
   basicNoteHeadProperties \push #'staff-position = #-6
   basicDotsProperties \push #'transparent = ##t
   squashedPosition = #-4
   basicStemProperties \push #'length = #12
}
\translator {
\ScoreContext
\accepts TabStaff;
}
}
}
