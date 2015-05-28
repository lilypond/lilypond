
\version "2.19.21"

\header {
  
  texidoc = "If note head is `over' the center line, the stem is
 shortened.  This happens with forced stem directions, and with some
 chord configurations."

}

\layout  {
  ragged-right = ##t
}


\relative {
  \stemDown d'' c b a g f e
  \stemNeutral
  \stemUp a b c d e f g a
  \stemNeutral
  s4 |
  
  <g, e'>2 <b e g> <c e g> <d f g>
  
} 


