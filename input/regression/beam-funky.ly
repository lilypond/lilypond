
\version "2.1.7"
\header {
    
    texidoc = "Knee beaming, complex configurations.  According to
Paul Roberts, the first stem of a beam determines the direction of the
beam, and as such the way that following (kneed) stems attach to the
beam. This is in disagreement with the current algorithm."
    
}

\score { \notes
\relative c'
	 {
	     \property Voice.Beam \set #'auto-knee-gap = #7
	     \property Voice.subdivideBeams = ##t	     
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
\paper { raggedright = ##t} 

     }

