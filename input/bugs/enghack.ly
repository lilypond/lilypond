\score {
       \notes {
         c'\longa c'\breve  
         c'1 c'2 c'4 c'8 c'16 c'32 c'64 c'64 c'2. c'8. c'16
         }
         \paper {
                linewidth = -1.0;
                \translator { \type "Score_engraver";
                            \name "Score";
                            \consists "Note_heads_engraver";
                            \consists "Stem_engraver";
                            \consists "Rhythmic_column_engraver";
         }}}
         
