\version "2.11.51"
\header {
  texidoc = "Note heads in collisions should be merged if
they have the same positions in the extreme note heads.  
"

}
\layout {ragged-right = ##t}



\relative c'' {
  c4 
  << { c4 d4 <c d>8  <c d> <c d> } \\  { c4 c <a b>8 <b c> <c d>8 } >>
}
 


