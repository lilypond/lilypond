\version "2.6.0"
\header {
  texidoc = "Note heads in collisions should be merged if
they have the same positions in the extreme note heads.  
"

}


\relative c'' {
  c4 
  << { c4 d4 <c d>8  <c d> <c d> } \\  { c4 c <a b>8 <b c> <c d>8 } >>
}
\layout {raggedright = ##t} 


