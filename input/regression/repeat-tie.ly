\header {

  texidoc = "Repeat ties are only connected on the right side to a
note head."

}

\version "2.11.51"
\paper {
  ragged-right = ##t
}

\relative c'' {
  r4 c4\repeatTie r <c d f  g> \repeatTie

} 
