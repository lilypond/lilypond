\version "2.1.26"
\header {
    texidoc = "There are three different kinds of grace types: the base 
grace switches to smaller type, the appoggiatura inserts also a slur, and the
acciaccatura inserts a slur and slashes the stem." 
    }

\score  {
    \notes \relative c'' { c4 \grace { d8 }  c4
			   \appoggiatura { d8 } c
			   \acciaccatura { d } c
		       }
    
}
 
