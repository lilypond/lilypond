
\version "2.3.17"

\header {
    texidoc = "Accidentals do not influence the amount of stretchable space.
The accidental does add a little non-stretchable space. 
"
}

\score {
      \relative c'' \context GrandStaff {
	#(set-accidental-style 'piano-cautionary)
	d16 d d d d d cis d 
	
     }

     %% not raggedright!!
     \paper {  linewidth = 14.\cm
     }
}
