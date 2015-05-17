
\version "2.19.21"
\header {
  texidoc = "Bar line should come before the grace note."
}
\layout { ragged-right = ##t}






\relative \context Staff  { 
  f'1 \grace {  a'16 f  } g1 }



