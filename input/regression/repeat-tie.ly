\header {

  texidoc = "Repeat ties are only connected on the right side to a
note head."

}

\version "2.19.21"
\paper {
  ragged-right = ##t
}

\relative {
  r4 c''4\repeatTie r <c d f  g> \repeatTie

} 
