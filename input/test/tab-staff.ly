
\version "1.3.110";

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
  basicStaffSymbolProperties \override #'line-count = #6
  \remove "Clef_engraver";
  \remove "Time_signature_engraver";
  \consists "Pitch_squash_engraver";
   basicNoteHeadProperties \override #'transparent = ##t
   basicStemProperties \override #'transparent = ##t
   basicNoteHeadProperties \override #'staff-position = #-6
   basicDotsProperties \override #'transparent = ##t
   squashedPosition = #-4
   basicStemProperties \override #'length = #12
}
\translator {
\ScoreContext
\accepts TabStaff;
}
}
}
