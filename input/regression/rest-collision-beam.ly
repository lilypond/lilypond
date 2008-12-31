\header {

  texidoc = "Rests under beams are shifted upon
collision."

}
\version "2.12.0"
\paper {
  ragged-right = ##t
}
  
\relative c''' {
  \stemDown b8[ r b]
  \stemUp b,,8[ r b] 
}

