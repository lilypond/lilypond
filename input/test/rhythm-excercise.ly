\header {

    texidoc = " a way to generate rhythm exercises with lilypond
    (e.g. no staff but retaining the barlines"


}

\score { \notes { c4 c4 [ c8 c8 ] }

	 \paper {
	     \translator { \StaffContext
			   \remove Staff_symbol_engraver
			   \consists Pitch_squash_engraver
			   \remove Clef_engraver
		       }
	 }
     }
