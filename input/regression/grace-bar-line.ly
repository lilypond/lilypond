
\version "2.12.0"
\header {
  texidoc = "Bar line should come before the grace note."
}
\layout { ragged-right = ##t}






\relative c' \context Staff  { 
  f1 \grace {  a'16[ f]  } g1 }



