\version "2.11.51"
\header {
    
    texidoc = "If dotted note heads must remain on the left side,  
collision resolution moves the dots to the right."

}

\layout { ragged-right = ##t }

\relative c {
  \key d \minor
  \clef bass
  << <cis a' cis>4 \\ { g'8. bes16} >>
}
