
\version "2.1.25"

\header {
    texidoc = "Accidentals do not influence the amount of stretchable space. "
}

\score {
     \notes \relative c'' \context GrandStaff {
	#(set-accidental-style 'piano-cautionary)
	d16 d d d d d cis d 
	
     }

     %% not raggedright!!
     \paper {  linewidth = 14.\cm
     }
}
