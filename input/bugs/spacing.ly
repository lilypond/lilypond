\score {
    \notes \transpose c'' \context Staff { c\longa*1/4 c\breve*1/2 c1 c2 c4 c8 c16 c32 c64 }
    \paper {
     \translator {
       \StaffContext
      % \remove "Staff_symbol_engraver";
        \remove "Time_signature_engraver";
        \remove "Bar_engraver";
        \remove "Clef_engraver";
 }
linewidth = -1.;
    }}


