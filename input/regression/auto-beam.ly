\header {
  texidoc="Beams are placed automatically; the last measure should have a single
beam."
}

\version "2.19.21"

\layout  {
  ragged-right = ##t 
  \context {
    \Staff
    \remove "Clef_engraver"
  }
}


\relative {
  a'\longa a\breve  
  a1 a2 a4 a8 a16 a32 a64 a64 
}
