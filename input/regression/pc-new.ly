\score {
  \notes \relative c'' {

      \newpartcombine
      #(list
	(cons (ly:make-moment 2 4) 'together) 
	(cons (ly:make-moment 4 4) 'apart) 
			
    ) {
	  g2 g g
	  }
      { f f f }
      }

  \paper  {
      \translator { \VoiceContext
		    \denies Thread
		    \consists Note_heads_engraver
		    \consists Rest_engraver
		    }
      
      
      }
}
