\score{
    \context StaffGroup < 
    %< 
	  \context Staff=one \notes\relative c''{
	      f,
	      <f, a c>
	  }
	  \context Staff=two \notes\relative c{
	      \clef bass;
	      g
	      <g b d>
	  }
    >
    \paper{
    	\translator{
	    \StaffContext
	    \consists Arpeggio_engraver;
	}
    }
}
