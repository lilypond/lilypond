\version "1.5.68"
\header {
    texidoc = "Knee beaming. (funky)"
}

\score { \notes
\relative c'
	 {
	     \property Voice.Beam \set #'auto-knee-gap = #7
	     \property Voice.subdivideBeams = ##t	     
	     \time 8/8
	     [c,16  g'''16  
	     c,,,16  g'''16 
	     c,,,16  g'''16 	     	     
	     c,,,16  g'''16 ]

	     [c,,,16 c c c
	      b'''   b b b]

	     \transpose a, { \relative b'' { [g,16  g'''16  
	     g,,,16  g'''16 
	     g,,,16  g'''16 	     	     
	     g,,,16  g'''16
	     g g,,,
	     g''' g,,,
	     g''' g,,,
	     g''' g,,,]	    }  }
	     
	 }
\paper { linewidth = -1. } 

     }
