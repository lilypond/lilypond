
\version "2.12.0"
\header {
  texidoc = "Pieces may begin with grace notes."
}

\layout { ragged-right = ##t}

\relative c' \context Staff  { 
  \grace {  a'16[ f]  } g1
  \bar "||"  % test if | and || are synced.
  \grace {  a16[ bes]  }  c1
  \bar "||"  % test if | and || are synced. 
}



