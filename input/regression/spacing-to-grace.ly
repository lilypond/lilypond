\header {

    texidoc ="space from a normal note /barline to a grace note is
    smaller than to a normal note."
    
}

\score {
    \context Voice \notes
    { \time 2/4
      \relative c'' {
	  e8 e \grace d8 e e \grace f8 e es, d' d
	  e8 e e e \grace { \stemDown f8 \stemBoth } e es, d'

		  }  

    }
    \paper { linewidth =  -1.0 }
    }
