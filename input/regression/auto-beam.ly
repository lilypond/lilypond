\header {
  texidoc="Beams are place automatically; the last measure should have a single 
beam."
}

\version "2.6.0"

\layout  { raggedright = ##t } 

\relative c'' {
  a\longa a\breve  
  a1 a2 a4 a8 a16 a32 a64 a64 
}
\layout {
  \context {
    \Staff
    \remove "Clef_engraver"
  }
}

