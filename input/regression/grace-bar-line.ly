
\version "2.6.0"
\header {
texidoc = "Bar line should come before the grace note."
}

	



\score  { \relative c' \context Staff  { 
 f1 \grace {  a'16[ f]  } g1 }
		\layout { raggedright = ##t}
 } 

