\version "1.3.146"

\score { 
  \context Voice \notes\relative c {
    \time 4/4
	g''1 a2 b4. c8
	\repeat "volta" 2 { a4 b c d } \alternative { { c c c c } { d2 d4 [d8 d] } }
	
  }
  \paper { }  
  \midi { }
}
