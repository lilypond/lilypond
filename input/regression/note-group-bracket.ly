
\header {

    texidoc = "Note grouping events are used to indicate where
brackets for analysis start and end."
    
}



\score {
    \notes
    {
	
	c4-\groupOpen-\groupOpen-\groupOpen
	c4-\groupClose
	c4-\groupOpen
	c4-\groupClose-\groupClose
	c4-\groupOpen
	c4-\groupClose-\groupClose
    }

    \paper {
	\translator {
	    \StaffContext \consists "Horizontal_bracket_engraver"
	}

	linewidth = -1.0
    }
}
