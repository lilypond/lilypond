
\version "1.9.4"

\header {
    texidoc = "Accidentals don't influence the amount of stretchable space. "
}

\score {
     \notes \relative c'' \context GrandStaff {
	\pianoCautionaries
	d16 d d d d d cis d 
	
     }

     %% not raggedright!!
     \paper {  linewidth = 14.\cm
     }
}
