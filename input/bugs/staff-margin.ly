voice = \notes\relative c'{
  \property Staff.instrument	= "Voice"
  \property Staff.instr		= "V."

  % Staff_margin_engraver kan be `fixed' by doing a silly request:
  %\bar ".|";

  c1\break c \bar "|.";
} 

soprano = \notes\relative c'{
  \property Staff.instrument	= "Soprano"
  \property Staff.instr		= "S."
  c1 c \bar "|.";
} 

tenor = \notes \relative c'{
  \property Staff.instrument	= "Tenor"
  \property Staff.instr		= "T."
  c1 c
}

\score
{
  \context StaffGroup< 
    \context Staff = "voice"	\voice
    \context GrandStaff< 
      \context Staff = "soprano" \soprano
      \context Staff = "tenor"	\tenor
    >
  >

  \paper {
    indent = 50.0\mm;
    linewidth = 100.0\mm;
    \translator { \StaffContext \consists Staff_margin_engraver; }
  }
}
