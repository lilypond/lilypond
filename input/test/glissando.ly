\header{
texidoc="
Simple glissando lines between notes are supported.  The first two glissandi are not consecutive.
";
}

\score{
        \context Staff=one \notes\relative c''{
	     % gliss non gliss and 
	     c \glissando d e \glissando f\break
	     % consecutive 
	     c \glissando d \glissando e f
    }
    \paper{
        linewidth = 70.\mm;
	\translator{
	       \StaffContext
	       % makes for handier debugging
	       % \remove Clef_engraver;
	}
    }
}