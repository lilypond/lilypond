
\version "2.7.13"
\header {
  texidoc = "Pieces may begin with grace notes."
}

\layout { raggedright = ##t}

\relative c' \context Staff  { 
  \grace {  a'16[ f]  } g1
  \bar "||"  % test if | and || are synced.
  \grace {  a16[ bes]  }  c1
  \bar "||"  % test if | and || are synced. 
}



