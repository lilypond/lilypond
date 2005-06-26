
\version "2.6.0"
\header {
texidoc = "Pieces may begin with grace notes."
}
\score  { \relative c' \context Staff  { 
 \grace {  a'16[ f]  } g1
 \bar "||"  % test if | and || are synced.
 \grace {  a16[ bes]  }  c1
 \bar "||"  % test if | and || are synced. 
  }
  \layout { raggedright = ##t}
}

