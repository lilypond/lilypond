
\version "2.3.22"
\header {
    
    texidoc = "In complex configurations of knee beaming, according to
Paul Roberts, the first stem of a beam determines the direction of the
beam, and as such the way that following (kneed) stems attach to the
beam. This is in disagreement with the current algorithm."
    
}

\score { 
\relative c'
	 {
	     \override Beam  #'auto-knee-gap = #7
	     \set subdivideBeams = ##t	     
	     \time 8/8
	      c,16[  g'''16  
	     c,,,16  g'''16 
	     c,,,16  g'''16 	     	     
	     c,,,16  g'''16 ]

	      c,,,16[ c c c
	      b'''   b b b]

	     \transpose c' a, { \relative b'' {  g,16[  g'''16  
	     g,,,16  g'''16 
	     g,,,16  g'''16 	     	     
	     g,,,16  g'''16
	     g g,,,
	     g''' g,,,
	     g''' g,,,
	     g''' g,,,]	    }  }
	     
	 }
\layout { raggedright = ##t} 

     }

