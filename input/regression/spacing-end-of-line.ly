
\version "2.6.0"
\header {
  
  texidoc ="Broken engraving of a bar at the end of a line does not upset 
    the space following rests and notes."

}

\layout {
  raggedright = ##t }


\relative c' {
  c2.. r8
  c2.. r8
  \time 3/4 \break
  e2 e4 | e2 e4 \time 4/4 \break
}
  


