
\version "1.9.4"
\header {
texidoc = "Bar line should come before the grace note."
}

	



\score  {\notes \relative c' \context Staff  { 
 f1 \grace {  a'16[ f]  } g1 }
		\paper { raggedright = ##t}
 } 

