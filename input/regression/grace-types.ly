\version "1.9.2"
\header {
    texidoc = "Different grace types explained: the base grace switches to smaller type. The appoggiatura also inserts a slur, and the
accacciatura inserts a slur and slashes the stem." 
    }

\score  {
    \notes \relative c'' { c4 \grace { d8 }  c4
			   \appoggiatura { d8 } c
			   \accacciatura { d } c
		       }
    
}
