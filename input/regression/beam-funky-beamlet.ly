#(ly:set-option 'old-relative)
\version "1.9.1"
\header {

texidoc=" Funky kneed beams with beamlets also work. The beamlets
should be pointing to the note head.
"

}


\score {
  \notes\relative c' {
     c16 c''8 c,,16
     c16 c''8 c16
     c16 c,,8 c16          
     
  }
  \paper { raggedright = ##t}
}
	  
