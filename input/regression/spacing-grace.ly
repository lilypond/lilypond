
\version "2.3.4"
\header {
  texidoc = "Grace note spacing. Should it be tuned? "
}
	
\score {
  \context Voice \relative c'' { \grace {  c16[ d] } c4 }
  \paper { raggedright = ##t}

}

